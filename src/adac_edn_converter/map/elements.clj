(ns adac-edn-converter.map.elements
  "Map XSD particles / complexTypes → SchemaCraft Element records.
  Shared shapes (same name/docs/typedef/cardinality/children) reuse one Element."
  (:require [adac-edn-converter.map.typedefs :as td]
            [adac-edn-converter.util :as u]
            [clojure.string :as str]))

(def ^:dynamic *include-child?*
  "Optional (fn [parent-path child-name] boolean). Nil = include all."
  nil)

(defn- include-child?
  [parent-path child-name]
  (if *include-child?*
    (*include-child?* parent-path child-name)
    true))

(defn empty-store
  "Element store: {:by-id {uuid → element} :by-shape {fingerprint → uuid}}."
  []
  {:by-id {} :by-shape {}})

(defn store-elements
  "All unique Element records in the store."
  [store]
  (vals (:by-id @store)))

(defn normalize-doc
  "Trim and collapse whitespace per line; preserve newlines between paragraphs."
  [d]
  (when d
    (let [lines (->> (str/split-lines (str d))
                     (map (fn [line]
                            (-> line str/trim (str/replace #"\s+" " "))))
                     (remove str/blank?))]
      (when (seq lines)
        (str/join "\n" lines)))))

(def geometry-host-action-base
  "Shared CAD capture action metadata for Geometry host-events."
  {:heading "Geometry"
   :label "Capture geometry from CAD"
   :handler "onCaptureGeometry"})

(defn geometry-host-event?
  "True when this is an ADAC Geometry field backed by a geometry_* complex type."
  [name type-ref]
  (and (= "Geometry" (u/local-name name))
       (let [t (u/local-name type-ref)]
         (and (string? t) (str/starts-with? t "geometry_")))))

(defn element-shape
  "Canonical fingerprint for Element reuse (no path / order)."
  [{:keys [name kind documentation cardinality nillable?
           placement property-name typedef-id child-refs item-ref host-action fixed]}]
  (cond-> {:name (u/local-name name)
           :kind kind
           :documentation (normalize-doc documentation)
           :cardinality cardinality
           :nillable? (boolean nillable?)}
    placement (assoc :placement placement)
    property-name (assoc :property-name property-name)
    typedef-id (assoc :typedef-id typedef-id)
    (seq child-refs) (assoc :child-refs (vec child-refs))
    item-ref (assoc :item-ref item-ref)
    host-action (assoc :host-action host-action)
    fixed (assoc :fixed fixed)))
(defn- shape-id
  [shape]
  (u/stable-uuid (str "element/shape/" (pr-str shape))))

(defn find-or-create-element!
  "Reuse existing Element when shape matches; otherwise mint id from shape and store."
  [store shape build-fn]
  (if-let [existing-id (get-in @store [:by-shape shape])]
    (get-in @store [:by-id existing-id])
    (let [id (shape-id shape)
          el (build-fn id)]
      (swap! store (fn [s]
                     (-> s
                         (assoc-in [:by-id id] el)
                         (assoc-in [:by-shape shape] id))))
      el)))

(defn collection-wrapper?
  "ADAC pattern: sequence of a single unbounded element child."
  [particle]
  (and (= :sequence (:kind particle))
       (= 1 (count (:particles particle)))
       (let [c (first (:particles particle))]
         (and (= :element (:kind c))
              (= :many (:max-occurs c))))))

(defn flatten-particles
  "Expand group-refs and unwrap single nested sequences for walking."
  [schema particles]
  (mapcat
   (fn [p]
     (case (:kind p)
       :group-ref
       (let [g (get-in schema [:groups (u/local-name (:ref p))])
             inner (:particle g)]
         (if inner
           (let [expanded (flatten-particles schema [inner])
                 max-o (:max-occurs p)
                 min-o (:min-occurs p)]
             (if (and (or (= :many max-o) (and (number? max-o) (> max-o 1)))
                      (= 1 (count expanded))
                      (= :choice (:kind (first expanded))))
               [(assoc (first expanded)
                       :min-occurs min-o
                       :max-occurs max-o)]
               expanded))
           []))
       :sequence [p]
       [p]))
   particles))

(defn resolve-complex-particles
  "Flatten complexContent extension bases into a single particle list."
  [schema type-name]
  (let [local (u/local-name type-name)
        ct (get-in schema [:complex-types local])]
    (when ct
      (case (:kind ct)
        :simple-content
        [{:kind :simple-content
          :base (:base ct)
          :attributes (:attributes ct)
          :documentation (:documentation ct)}]
        (let [base-parts (when (:base ct)
                           (resolve-complex-particles schema (:base ct)))
              own (mapcat
                   (fn [p]
                     (case (:kind p)
                       :sequence (:particles p)
                       :choice [p]
                       [p]))
                   (or (:particles ct) []))
              attrs (:attributes ct)]
          (cond-> (vec (concat base-parts own))
            (seq attrs) (into attrs)))))))

(defn- ensure-typedef!
  [*typedefs schema-id simple-types type-name]
  (let [local (if (u/xs-qname? type-name)
                (str "xs_" (u/local-name type-name))
                (u/local-name type-name))
        store-key (if (u/xs-qname? type-name)
                    (str "xs:" (u/local-name type-name))
                    local)]
    (when-not (contains? @*typedefs store-key)
      (let [tdef (if (u/xs-qname? type-name)
                   (let [t (td/emit-builtin-typedef schema-id type-name)]
                     (assoc t
                            :record/name (u/as-keyword local)
                            :element/key (u/as-keyword local)
                            :record/id (td/typedef-id schema-id local)
                            :record/label (u/element-label (u/local-name type-name))
                            :element/label (u/element-label (u/local-name type-name))))
                   (td/emit-typedef schema-id simple-types type-name))]
        (swap! *typedefs assoc store-key tdef)))
    (get @*typedefs store-key)))

(declare emit-complex-type! emit-element-decl!)

(defn- cardinality
  [min-occurs max-occurs & {:keys [collection?]}]
  {:min (or min-occurs 0)
   :max (if (or (= :many max-occurs) collection?)
          :many
          (or max-occurs 1))})

(defn register-scalar!
  [store {:keys [schema-id name documentation nillable? min-occurs max-occurs
                 typedef-id order placement property-name fixed]}]
  (let [card (cardinality min-occurs max-occurs)
        shape (element-shape
               {:name name
                :kind :scalar
                :documentation documentation
                :cardinality card
                :nillable? nillable?
                :placement placement
                :property-name property-name
                :typedef-id typedef-id
                :fixed fixed})]
    (find-or-create-element!
     store shape
     (fn [id]
       (let [kw (u/as-keyword name)
             label (u/element-label name)
             data (cond-> {:type/ref typedef-id}
                    nillable? (assoc :nillable? true)
                    placement (assoc :placement placement)
                    property-name (assoc :property-name property-name)
                    fixed (assoc :fixed fixed))]
         (cond-> {:record/type :element
                  :record/id id
                  :schema/id schema-id
                  :record/parent-id schema-id
                  :record/name kw
                  :record/label label
                  :record/documentation (normalize-doc documentation)
                  :element/type :simple
                  :element/key kw
                  :element/kind :scalar
                  :element/label label
                  :element/documentation (normalize-doc documentation)
                  :element/typedef-id typedef-id
                  :element/child-refs nil
                  :element/cardinality card
                  :element/order (or order 0)
                  :element/children []
                  :element/status :active
                  :element/data data}
           nillable? (assoc :element/nillable? true)
           placement (assoc :element/placement placement)
           property-name (assoc :element/property-name property-name)
           fixed (assoc :element/fixed fixed)))))))

(defn register-complex!
  [store {:keys [schema-id name documentation kind min-occurs max-occurs
                 child-refs order item-ref]}]
  (let [collection? (= kind :collection)
        card (cardinality min-occurs max-occurs :collection? collection?)
        shape (element-shape
               {:name name
                :kind kind
                :documentation documentation
                :cardinality card
                :nillable? false
                :child-refs child-refs
                :item-ref item-ref})]
    (find-or-create-element!
     store shape
     (fn [id]
       (let [kw (u/as-keyword name)
             label (u/element-label name)
             data (cond-> {:child-refs child-refs}
                    item-ref (assoc :collection/item-ref item-ref))]
         {:record/type :element
          :record/id id
          :schema/id schema-id
          :record/parent-id schema-id
          :record/name kw
          :record/label label
          :record/documentation (normalize-doc documentation)
          :element/type :complex
          :element/key kw
          :element/kind kind
          :element/label label
          :element/documentation (normalize-doc documentation)
          :element/typedef-id nil
          :element/child-refs child-refs
          :element/cardinality card
          :element/order (or order 0)
          :element/children []
          :element/status :active
          :element/data data})))))

(defn register-host-event!
  "Register Geometry as a CAD host-event with exactly one ordinary Target."
  [store {:keys [schema-id name documentation min-occurs max-occurs
                 target-id order]}]
  (let [card (cardinality min-occurs max-occurs)
        host-action (assoc geometry-host-action-base :target-id target-id)
        child-refs [target-id]
        shape (element-shape
               {:name name
                :kind :host-event
                :documentation documentation
                :cardinality card
                :nillable? false
                :child-refs child-refs
                :host-action host-action})]
    (find-or-create-element!
     store shape
     (fn [id]
       (let [kw (u/as-keyword name)
             label (u/element-label name)
             doc (normalize-doc documentation)]
         {:record/type :element
          :record/id id
          :schema/id schema-id
          :record/parent-id schema-id
          :record/name kw
          :record/label label
          :record/documentation doc
          :element/type :host-event
          :element/key kw
          :element/kind :host-event
          :element/label label
          :element/documentation doc
          :element/typedef-id nil
          :element/child-refs child-refs
          :element/cardinality card
          :element/order (or order 0)
          :element/children []
          :element/status :active
          :element/host-action host-action
          :element/data {:child-refs child-refs
                         :host-action host-action}})))))
(defn emit-attribute!
  [store *typedefs schema schema-id parent-path attr order]
  (let [simple-types (:simple-types schema)
        tdef (ensure-typedef! *typedefs schema-id simple-types
                              (or (:type-ref attr) "xs:string"))
        name (:name attr)
        min-o (if (= "required" (:use attr)) 1 0)]
    (register-scalar!
     store
     {:schema-id schema-id
      :name name
      :documentation (:documentation attr)
      :nillable? false
      :min-occurs min-o
      :max-occurs 1
      :typedef-id (:record/id tdef)
      :order order
      :placement :parent-property
      :property-name name
      :fixed (:fixed attr)})))

(defn- emit-simple-content!
  [store *typedefs schema schema-id path name documentation sc order]
  (let [simple-types (:simple-types schema)
        content-tdef (ensure-typedef! *typedefs schema-id simple-types
                                      (or (:base sc) "xs:string"))
        content-el (register-scalar!
                    store
                    {:schema-id schema-id
                     :name (str name "_content")
                     :documentation "Simple content value"
                     :nillable? false
                     :min-occurs 1
                     :max-occurs 1
                     :typedef-id (:record/id content-tdef)
                     :order 0})
        attr-els (map-indexed
                  (fn [i attr]
                    (emit-attribute! store *typedefs schema schema-id path attr (inc i)))
                  (:attributes sc))
        child-refs (into [(:record/id content-el)] (mapv :record/id attr-els))]
    (register-complex!
     store
     {:schema-id schema-id
      :name name
      :documentation documentation
      :kind :sequence
      :min-occurs 0
      :max-occurs 1
      :child-refs child-refs
      :order order})))

(defn- register-geometry-or-complex!
  "Register Geometry as host-event when requested; otherwise as ordinary complex."
  [store {:keys [host-event-owner? schema-id name documentation kind
                 min-occurs max-occurs child-refs item-ref order path]}]
  (if-not host-event-owner?
    (register-complex!
     store
     {:schema-id schema-id
      :name name
      :documentation documentation
      :kind kind
      :min-occurs min-occurs
      :max-occurs max-occurs
      :child-refs child-refs
      :item-ref item-ref
      :order order})
    (do
      (when-not (= 1 (count child-refs))
        (throw (ex-info
                "Geometry host-event requires exactly one Target child"
                {:path path
                 :name name
                 :child-refs child-refs
                 :kind kind})))
      (register-host-event!
       store
       {:schema-id schema-id
        :name name
        :documentation documentation
        :min-occurs min-occurs
        :max-occurs (if (= :many max-occurs) 1 max-occurs)
        :target-id (first child-refs)
        :order order}))))

(defn emit-complex-type!
  "Emit a sequence/choice/collection element for a named or anonymous complex type.
  When `:host-event-owner?` is true, the owner is registered as a Geometry host-event
  around its single ordinary Target child."
  [store *typedefs schema schema-id path name documentation type-ref-or-inline order
   & {:keys [min-occurs max-occurs host-event-owner?]}]
  (let [inline? (map? type-ref-or-inline)
        type-name (when-not inline? type-ref-or-inline)
        ct (if inline?
             type-ref-or-inline
             (get-in schema [:complex-types (u/local-name type-name)]))]
    (cond
      (nil? ct)
      (throw (ex-info "Unknown complex type" {:type type-name :path path}))

      (= :simple-content (:kind ct))
      (emit-simple-content! store *typedefs schema schema-id path name
                            (or documentation (:documentation ct)) ct order)

      :else
      (let [raw-parts (if inline?
                        (mapcat (fn [p]
                                  (case (:kind p)
                                    :sequence (:particles p)
                                    [p]))
                                (or (:particles ct) []))
                        (or (resolve-complex-particles schema type-name) []))
            coll? (and (= 1 (count raw-parts))
                       (= :element (:kind (first raw-parts)))
                       (= :many (:max-occurs (first raw-parts))))
            wrapper-seq (when inline?
                          (first (filter #(and (= :sequence (:kind %))
                                               (collection-wrapper? %))
                                         (:particles ct))))
            item (when (or coll? wrapper-seq)
                   (if wrapper-seq
                     (first (:particles wrapper-seq))
                     (first raw-parts)))]
        (if item
          (let [item-el (emit-element-decl! store *typedefs schema schema-id
                                            (conj path (:name item)) item 0)]
            (register-geometry-or-complex!
             store
             {:host-event-owner? host-event-owner?
              :schema-id schema-id
              :name name
              :documentation (or documentation (:documentation ct))
              :kind :collection
              :min-occurs (or min-occurs 1)
              :max-occurs :many
              :child-refs [(:record/id item-el)]
              :item-ref (:record/id item-el)
              :order order
              :path path}))
          (let [only (when (= 1 (count raw-parts)) (first raw-parts))
                choice? (= :choice (:kind only))
                unbounded-choice? (and choice? (= :many (:max-occurs only)))
                child-particles (filter
                                 (fn [p]
                                   (or (not= :element (:kind p))
                                       (include-child? path (:name p))))
                                 (if choice?
                                   (:particles only)
                                   (remove #(= :attribute (:kind %)) raw-parts)))
                attrs (filter #(= :attribute (:kind %)) raw-parts)
                child-els (vec
                           (map-indexed
                            (fn [i p]
                              (case (:kind p)
                                :element
                                (emit-element-decl! store *typedefs schema schema-id
                                                    (conj path (:name p)) p i)
                                :choice
                                (let [ch-path (conj path (str "choice_" i))
                                      ch-children
                                      (vec
                                       (map-indexed
                                        (fn [j c]
                                          (emit-element-decl! store *typedefs schema schema-id
                                                              (conj ch-path (:name c)) c j))
                                        (:particles p)))]
                                  (register-complex!
                                   store
                                   {:schema-id schema-id
                                    :name (str name "_choice_" i)
                                    :documentation nil
                                    :kind :choice
                                    :min-occurs (:min-occurs p)
                                    :max-occurs (if (= :many (:max-occurs p)) 1 (:max-occurs p))
                                    :child-refs (mapv :record/id ch-children)
                                    :order i}))
                                :sequence
                                (let [seq-path (conj path (str "seq_" i))
                                      seq-children
                                      (vec
                                       (map-indexed
                                        (fn [j c]
                                          (emit-element-decl! store *typedefs schema schema-id
                                                              (conj seq-path (:name c)) c j))
                                        (:particles p)))]
                                  (register-complex!
                                   store
                                   {:schema-id schema-id
                                    :name (str name "_seq_" i)
                                    :documentation nil
                                    :kind :sequence
                                    :min-occurs (:min-occurs p)
                                    :max-occurs (:max-occurs p)
                                    :child-refs (mapv :record/id seq-children)
                                    :order i}))
                                nil))
                            child-particles))
                attr-els (map-indexed
                          (fn [i a]
                            (emit-attribute! store *typedefs schema schema-id path a
                                             (+ (count child-els) i)))
                          (concat attrs (:attributes ct)))
                kind (cond
                       unbounded-choice? :collection
                       choice? :choice
                       :else :sequence)
                choice-item (when unbounded-choice?
                              (register-complex!
                               store
                               {:schema-id schema-id
                                :name (str name "_Fragment")
                                :documentation "Ordered heterogeneous choice item"
                                :kind :choice
                                :min-occurs 1
                                :max-occurs 1
                                :child-refs (mapv :record/id (remove nil? child-els))
                                :order 0}))
                child-refs (if unbounded-choice?
                             [(:record/id choice-item)]
                             (into (mapv :record/id (remove nil? child-els))
                                   (mapv :record/id attr-els)))]
            (register-geometry-or-complex!
             store
             {:host-event-owner? host-event-owner?
              :schema-id schema-id
              :name name
              :documentation (or documentation (:documentation ct))
              :kind kind
              :min-occurs (or min-occurs 0)
              :max-occurs (if unbounded-choice? :many (or max-occurs 1))
              :child-refs child-refs
              :item-ref (when unbounded-choice? (:record/id choice-item))
              :order order
              :path path})))))))

(defn emit-element-decl!
  [store *typedefs schema schema-id path el-decl order]
  (let [simple-types (:simple-types schema)
        name (:name el-decl)
        doc (:documentation el-decl)
        min-o (:min-occurs el-decl)
        max-o (:max-occurs el-decl)
        nillable? (:nillable? el-decl)
        fixed (:fixed el-decl)]
    (cond
      (:inline-complex el-decl)
      (emit-complex-type! store *typedefs schema schema-id path name doc
                          (:inline-complex el-decl) order
                          :min-occurs min-o :max-occurs max-o)

      (:type-ref el-decl)
      (let [tref (:type-ref el-decl)
            local (u/local-name tref)]
        (cond
          (or (u/xs-qname? tref) (contains? (:simple-types schema) local))
          (let [tdef (ensure-typedef! *typedefs schema-id simple-types tref)]
            (register-scalar!
             store
             {:schema-id schema-id
              :name name
              :documentation doc
              :nillable? nillable?
              :min-occurs min-o
              :max-occurs max-o
              :typedef-id (:record/id tdef)
              :order order
              :fixed fixed}))

          (contains? (:complex-types schema) local)
          (emit-complex-type! store *typedefs schema schema-id path name doc
                              tref order
                              :min-occurs min-o
                              :max-occurs max-o
                              :host-event-owner? (geometry-host-event? name tref))

          :else
          (throw (ex-info "Unresolved element type" {:type tref :path path}))))

      (:inline-simple el-decl)
      (let [tdef (ensure-typedef! *typedefs schema-id simple-types "xs:string")]
        (register-scalar!
         store
         {:schema-id schema-id
          :name name
          :documentation doc
          :nillable? nillable?
          :min-occurs min-o
          :max-occurs max-o
          :typedef-id (:record/id tdef)
          :order order
          :fixed fixed}))

      :else
      (throw (ex-info "Element without type" {:element el-decl :path path})))))
