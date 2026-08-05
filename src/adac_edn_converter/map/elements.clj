(ns adac-edn-converter.map.elements
  "Map XSD particles / complexTypes → SchemaCraft Element records.
  Shared shapes (same name/docs/typedef/cardinality/children) reuse one Element."
  (:require [adac-edn-converter.map.typedefs :as td]
            [adac-edn-converter.util :as u]
            [clojure.string :as str]))

(def ^:dynamic *include-child?*
  "Optional (fn [parent-path child-name] boolean). Nil = include all."
  nil)

(def ^:dynamic *geometry-host-events?*
  "When true (ADAC profile), Geometry + geometry_* becomes :host-event."
  true)

(def ^:dynamic *segment-host-events?*
  "When true (LandXML profile), GeomList segment types become :self host-events."
  false)

(def ^:dynamic *flatten-type-choices?*
  "When true, flatten type-level unbounded choices into named sequence fields."
  false)

(def flatten-type-choice-names
  "LandXML types whose unbounded choice content becomes a flat form."
  #{"Alignment" "PlanFeature" "Parcel"})

(def ^:dynamic *emit-frames*
  "Map of emit-frame-key → {:name :type-key :id :hit?} for recursive element graphs."
  {})

(def ^:dynamic *current-emit-frame*
  "Frame for the element declaration currently being expanded."
  nil)

(defn- decl-type-key
  "Distinguish same-named elements with different types (e.g. Feature vs enum)."
  [el-decl]
  (cond
    (:type-ref el-decl) (str "type:" (u/local-name (:type-ref el-decl)))
    (:inline-complex el-decl) (str "ict:" (hash (pr-str (:inline-complex el-decl))))
    (:inline-simple el-decl) (str "ist:" (hash (pr-str (:inline-simple el-decl))))
    (:ref el-decl) (str "ref:" (u/local-name (:ref el-decl)))
    :else "unknown"))

(defn- emit-frame-key
  [name min-occurs max-occurs nillable? type-key]
  [name min-occurs max-occurs (boolean nillable?) type-key])

(defn- recursive-force-id
  "When a recursive ref hit this frame, reuse the preallocated id."
  [name]
  (when-let [frame *current-emit-frame*]
    (when (and (= (:name frame) name) @(:hit? frame))
      (:id frame))))

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
  (and *geometry-host-events?*
       (= "Geometry" (u/local-name name))
       (let [t (u/local-name type-ref)]
         (and (string? t) (str/starts-with? t "geometry_")))))

(def segment-host-names
  "LandXML CoordGeom / GeomList segment types that are CAD host-events."
  #{"Line" "Curve" "Spiral" "IrregularLine"})

(defn segment-host-event?
  "True when this LandXML segment type should be a :self host-event."
  [name]
  (and *segment-host-events?*
       (contains? segment-host-names (u/local-name name))))

(defn segment-host-action-base
  "CAD capture metadata for a LandXML segment host-event (:mode :self)."
  [name]
  (let [n (u/local-name name)]
    {:heading n
     :label (str "Capture " (str/lower-case n) " from CAD")
     :handler (str "onCapture" n)
     :mode :self}))

(defn- flatten-choice-to-fields
  "Expand choice/sequence wrappers into a flat list of element particles.
  Nested exclusive choices become optional sibling fields (practical type forms)."
  [particles]
  (mapcat
   (fn [p]
     (case (:kind p)
       :choice (flatten-choice-to-fields (:particles p))
       :sequence (flatten-choice-to-fields (:particles p))
       :element [p]
       []))
   (or particles [])))

(defn- should-flatten-type-choice?
  [name]
  (and *flatten-type-choices?*
       (contains? flatten-type-choice-names (u/local-name name))))

(defn- root-bag-collection?
  "LandXML document root stays an outer collection of fragments."
  [name]
  (= "LandXML" (u/local-name name)))

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
  "Reuse existing Element when shape matches; otherwise mint id from shape and store.
  Optional `force-id` is used for recursive elements (self/mutual refs)."
  ([store shape build-fn]
   (find-or-create-element! store shape build-fn nil))
  ([store shape build-fn force-id]
   (if-let [existing-id (get-in @store [:by-shape shape])]
     (get-in @store [:by-id existing-id])
     (let [id (or force-id (shape-id shape))
           el (build-fn id)]
       (swap! store (fn [s]
                      (-> s
                          (assoc-in [:by-id id] el)
                          (assoc-in [:by-shape shape] id))))
       el))))

(defn- multi-occurs?
  "True when maxOccurs allows more than one item (`unbounded` or numeric > 1)."
  [max-o]
  (or (= :many max-o)
      (and (number? max-o) (> max-o 1))))

(defn collection-wrapper?
  "ADAC pattern: sequence of a single multi-occurs element child."
  [particle]
  (and (= :sequence (:kind particle))
       (= 1 (count (:particles particle)))
       (let [c (first (:particles particle))]
         (and (= :element (:kind c))
              (multi-occurs? (:max-occurs c))))))

(defn flatten-particles
  "Expand group-refs and unwrap nested sequences for walking."
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
       :sequence (flatten-particles schema (:particles p))
       [p]))
   particles))

(defn resolve-element-decl
  "Resolve xs:element ref= against global elements; keep local occurs/nillable/docs overrides."
  [schema el-decl]
  (if-let [ref (:ref el-decl)]
    (let [target (get-in schema [:elements (u/local-name ref)])]
      (when-not target
        (throw (ex-info "Unresolved element ref" {:ref ref})))
      (-> target
          (assoc :min-occurs (:min-occurs el-decl)
                 :max-occurs (:max-occurs el-decl))
          (cond->
            (contains? el-decl :nillable?) (assoc :nillable? (:nillable? el-decl))
            (:documentation el-decl) (assoc :documentation (:documentation el-decl))
            (:fixed el-decl) (assoc :fixed (:fixed el-decl))
            (:default el-decl) (assoc :default (:default el-decl)))))
    el-decl))

(defn resolve-simple-content
  "Flatten simpleContent extension chains (attrs + ultimate simple base)."
  [schema sc]
  (loop [base (:base sc)
         attrs (vec (:attributes sc))
         documentation (:documentation sc)
         facets (:facets sc)
         seen #{}]
    (let [local (u/local-name base)]
      (cond
        (or (nil? local) (seen local))
        {:kind :simple-content
         :base (or base "xs:string")
         :attributes attrs
         :documentation documentation
         :facets facets}

        (contains? (:simple-types schema) local)
        {:kind :simple-content
         :base local
         :attributes attrs
         :documentation documentation
         :facets facets}

        (u/xs-qname? base)
        {:kind :simple-content
         :base base
         :attributes attrs
         :documentation documentation
         :facets facets}

        :else
        (if-let [ct (get-in schema [:complex-types local])]
          (if (= :simple-content (:kind ct))
            (recur (:base ct)
                   (into (vec (:attributes ct)) attrs)
                   (or documentation (:documentation ct))
                   (or facets (:facets ct))
                   (conj seen local))
            {:kind :simple-content
             :base (or base "xs:string")
             :attributes attrs
             :documentation documentation
             :facets facets})
          {:kind :simple-content
           :base (or base "xs:string")
           :attributes attrs
           :documentation documentation
           :facets facets})))))

(defn resolve-complex-particles
  "Flatten complexContent extension bases into a single particle list."
  [schema type-name]
  (let [local (u/local-name type-name)
        ct (get-in schema [:complex-types local])]
    (when ct
      (case (:kind ct)
        :simple-content
        [(resolve-simple-content schema ct)]
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

(defn- inline-complex-particles
  "Own particles for an inline complex type, merging complexContent base if present."
  [schema ct]
  (let [base-parts (when (:base ct)
                     (resolve-complex-particles schema (:base ct)))
        own (mapcat
             (fn [p]
               (case (:kind p)
                 :sequence (:particles p)
                 :choice [p]
                 :any []
                 [p]))
             (or (:particles ct) []))
        attrs (:attributes ct)]
    (cond-> (vec (concat (or base-parts []) own))
      (seq attrs) (into attrs))))

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
   :max (cond
          (= :many max-occurs) :many
          (number? max-occurs) max-occurs
          collection? :many
          :else 1)})

(defn- collection-max-occurs
  "List max from the XSD item particle (finite cap or :many)."
  [item]
  (let [m (:max-occurs item)]
    (cond
      (= :many m) :many
      (number? m) m
      :else :many)))

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
                 child-refs order item-ref nillable? force-id]}]
  (let [collection? (= kind :collection)
        card (cardinality min-occurs max-occurs :collection? collection?)
        shape (element-shape
               {:name name
                :kind kind
                :documentation documentation
                :cardinality card
                :nillable? nillable?
                :child-refs child-refs
                :item-ref item-ref})]
    (find-or-create-element!
     store shape
     (fn [id]
       (let [kw (u/as-keyword name)
             label (u/element-label name)
             data (cond-> {:child-refs child-refs}
                    item-ref (assoc :collection/item-ref item-ref))]
         (cond-> {:record/type :element
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
                  :element/data data}
           nillable? (assoc :element/nillable? true))))
     force-id)))

(defn register-host-event!
  "Register a CAD host-event.

  :target mode (default / ADAC): exactly one Target via `:target-id`.
  :self mode (LandXML segments): `:child-refs` are the host's own form children;
  `:host-action` includes `:mode :self` and omits `:target-id`."
  [store {:keys [schema-id name documentation min-occurs max-occurs
                 target-id child-refs order host-action-base]}]
  (let [card (cardinality min-occurs max-occurs)
        action-base (or host-action-base geometry-host-action-base)
        mode (or (:mode action-base) :target)
        self? (= mode :self)
        child-refs (if self?
                     (vec child-refs)
                     [(or target-id (first child-refs))])
        host-action (if self?
                      (-> action-base
                          (assoc :mode :self)
                          (dissoc :target-id))
                      (assoc (dissoc action-base :mode) :target-id (first child-refs)))
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
  (let [resolved (resolve-simple-content schema sc)
        simple-types (:simple-types schema)
        content-tdef (ensure-typedef! *typedefs schema-id simple-types
                                      (or (:base resolved) "xs:string"))
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
                  (:attributes resolved))
        child-refs (into [(:record/id content-el)] (mapv :record/id attr-els))]
    (register-complex!
     store
     {:schema-id schema-id
      :name name
      :documentation (or documentation (:documentation resolved))
      :kind :sequence
      :min-occurs 0
      :max-occurs 1
      :child-refs child-refs
      :order order
      :force-id (recursive-force-id name)})))

(defn- register-geometry-or-complex!
  "Register Geometry/segment as host-event when requested; otherwise ordinary complex.
  :self host-events keep all child-refs; :target (default) requires exactly one Target."
  [store {:keys [host-event-owner? host-action-base schema-id name documentation kind
                 min-occurs max-occurs child-refs item-ref order path nillable?]}]
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
      :nillable? nillable?
      :order order
      :force-id (recursive-force-id name)})
    (let [mode (or (:mode host-action-base) :target)
          self? (= mode :self)]
      (when (and (not self?) (not= 1 (count child-refs)))
        (throw (ex-info
                "Host-event requires exactly one Target child"
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
        :max-occurs (if (and (not self?) (= :many max-occurs)) 1 max-occurs)
        :target-id (when-not self? (first child-refs))
        :child-refs child-refs
        :order order
        :host-action-base host-action-base}))))

(defn emit-complex-type!
  "Emit a sequence/choice/collection element for a named or anonymous complex type.
  When `:host-event-owner?` is true, the owner is registered as a Geometry host-event
  around its single ordinary Target child. LandXML segment names may also become
  :self host-events when `*segment-host-events?*` is true."
  [store *typedefs schema schema-id path name documentation type-ref-or-inline order
   & {:keys [min-occurs max-occurs host-event-owner? nillable?]}]
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
      (let [raw-parts (->> (if inline?
                             (inline-complex-particles schema ct)
                             (or (resolve-complex-particles schema type-name) []))
                           (flatten-particles schema)
                           vec)
            ;; Skip xs:any wildcards in v1; keep attributes out of content detection
            content-parts0 (vec (remove #(or (= :any (:kind %))
                                             (= :attribute (:kind %)))
                                        raw-parts))
            attrs-from-parts (filter #(= :attribute (:kind %)) raw-parts)
            ;; Flatten type-level unbounded choices into named fields (Alignment/Parcel/…)
            content-parts (if (should-flatten-type-choice? name)
                            (vec
                             (mapcat
                              (fn [p]
                                (if (and (= :choice (:kind p))
                                         (multi-occurs? (:max-occurs p)))
                                  (flatten-choice-to-fields (:particles p))
                                  [p]))
                              content-parts0))
                            content-parts0)
            coll? (and (= 1 (count content-parts))
                       (= :element (:kind (first content-parts)))
                       (multi-occurs? (:max-occurs (first content-parts))))
            wrapper-seq (when inline?
                          (first (filter #(and (= :sequence (:kind %))
                                               (collection-wrapper? %))
                                         (:particles ct))))
            item (when (or coll? wrapper-seq)
                   (if wrapper-seq
                     (first (:particles wrapper-seq))
                     (first content-parts)))
            seg-host? (and (segment-host-event? name) (not host-event-owner?))
            as-host? (boolean (or host-event-owner? seg-host?))
            host-action (when seg-host? (segment-host-action-base name))]
        (cond
          item
          ;; List cardinality comes from the item particle; item Element is one instance.
          ;; Same local name as the wrapper (e.g. LandStabilisation/LandStabilisation) must not
          ;; reuse the wrapper's emit frame — that creates a self item-ref.
          (let [item-name (or (:name item) (:ref item))
                item-decl (assoc item :min-occurs 1 :max-occurs 1)
                item-resolved (resolve-element-decl schema item-decl)
                item-frame-key (emit-frame-key item-name 1 1 (:nillable? item-resolved)
                                               (decl-type-key item-resolved))
                item-el (binding [*emit-frames* (dissoc *emit-frames* item-frame-key)
                                  *current-emit-frame* nil]
                          (emit-element-decl! store *typedefs schema schema-id
                                              (conj path item-name) item-decl 0))]
            (register-geometry-or-complex!
             store
             {:host-event-owner? as-host?
              :host-action-base host-action
              :schema-id schema-id
              :name name
              :documentation (or documentation (:documentation ct))
              :kind :collection
              ;; SchemaCraft list :min is the item particle min (0 ⇒ empty list OK).
              :min-occurs (or (:min-occurs item) 0)
              :max-occurs (collection-max-occurs item)
              :child-refs [(:record/id item-el)]
              :item-ref (:record/id item-el)
              :nillable? nillable?
              :order order
              :path path}))

          :else
          (let [only (when (= 1 (count content-parts)) (first content-parts))
                choice? (= :choice (:kind only))
                unbounded-choice? (and choice? (multi-occurs? (:max-occurs only)))
                ;; Non-root sole unbounded choice with attrs → sequence wrapping {name}_list
                ;; (Alignment/Curve). No attrs (ADAC Pathways) → keep outer collection.
                wrap-sole-choice? (and unbounded-choice?
                                       (not (root-bag-collection? name))
                                       (seq attrs-from-parts))
                child-particles (filter
                                 (fn [p]
                                   (or (not= :element (:kind p))
                                       (include-child? path (or (:name p) (:ref p)))))
                                 (cond
                                   wrap-sole-choice? (:particles only)
                                   unbounded-choice? (:particles only)
                                   choice? (:particles only)
                                   :else content-parts))
                attrs attrs-from-parts
                emit-child-particle!
                (fn emit-child-particle!
                  ([parent-path parent-name i p]
                   (emit-child-particle! parent-path parent-name i p {}))
                  ([parent-path parent-name i p {:keys [as-choice-branch?]}]
                   (case (:kind p)
                     :element
                     (let [el-name (or (:name p) (:ref p))
                           p' (cond-> p
                                as-choice-branch? (assoc :min-occurs 1 :max-occurs 1))]
                       (if (and (not as-choice-branch?)
                                (multi-occurs? (:max-occurs p)))
                         (let [item-decl (assoc p :min-occurs 1 :max-occurs 1)
                               item-el (emit-element-decl! store *typedefs schema schema-id
                                                           (conj parent-path el-name) item-decl i)
                               resolved (resolve-element-decl schema p)
                               base-name (or (:name resolved) el-name)
                               coll-name (str base-name "_list")]
                           (register-complex!
                            store
                            {:schema-id schema-id
                             :name coll-name
                             :documentation (:documentation resolved)
                             :kind :collection
                             :min-occurs (or (:min-occurs p) 0)
                             :max-occurs (collection-max-occurs p)
                             :child-refs [(:record/id item-el)]
                             :item-ref (:record/id item-el)
                             :nillable? (:nillable? resolved)
                             :order i}))
                         (emit-element-decl! store *typedefs schema schema-id
                                             (conj parent-path el-name) p' i)))
                     :choice
                     (if (multi-occurs? (:max-occurs p))
                       (let [list-name (str parent-name "_list")
                             frag-name (str parent-name "_Fragment")
                             frag-path (conj parent-path "Fragment")
                             ch-children
                             (vec
                              (map-indexed
                               (fn [j c]
                                 (emit-child-particle! frag-path parent-name j c
                                                       {:as-choice-branch? true}))
                               (:particles p)))
                             frag (register-complex!
                                   store
                                   {:schema-id schema-id
                                    :name frag-name
                                    :documentation "Ordered heterogeneous choice item"
                                    :kind :choice
                                    :min-occurs 1
                                    :max-occurs 1
                                    :child-refs (mapv :record/id (remove nil? ch-children))
                                    :order 0})]
                         (register-complex!
                          store
                          {:schema-id schema-id
                           :name list-name
                           :documentation nil
                           :kind :collection
                           :min-occurs (or (:min-occurs p) 0)
                           :max-occurs (collection-max-occurs p)
                           :child-refs [(:record/id frag)]
                           :item-ref (:record/id frag)
                           :order i}))
                       (let [ch-path (conj parent-path (str "choice_" i))
                             ch-children
                             (vec
                              (map-indexed
                               (fn [j c]
                                 (emit-child-particle! ch-path parent-name j c
                                                       {:as-choice-branch? true}))
                               (:particles p)))]
                         (register-complex!
                          store
                          {:schema-id schema-id
                           :name (str parent-name "_choice_" i)
                           :documentation nil
                           :kind :choice
                           :min-occurs (:min-occurs p)
                           :max-occurs (or (:max-occurs p) 1)
                           :child-refs (mapv :record/id (remove nil? ch-children))
                           :order i})))
                     :sequence
                     (let [seq-path (conj parent-path (str "seq_" i))
                           seq-children
                           (vec
                            (map-indexed
                             (fn [j c]
                               (emit-child-particle! seq-path parent-name j c))
                             (:particles p)))]
                       (register-complex!
                        store
                        {:schema-id schema-id
                         :name (str parent-name "_seq_" i)
                         :documentation nil
                         :kind :sequence
                         :min-occurs (:min-occurs p)
                         :max-occurs (:max-occurs p)
                         :child-refs (mapv :record/id (remove nil? seq-children))
                         :order i}))
                     :group-ref
                     (let [expanded (flatten-particles schema [p])]
                       (when (= 1 (count expanded))
                         (emit-child-particle! parent-path parent-name i (first expanded)
                                               {:as-choice-branch? as-choice-branch?})))
                     nil)))
                ;; Root bag / wrapped sole choice: alternatives are card-1 choice branches.
                ;; Flattened types: emit as normal fields (multi → *_list).
                ;; Unbounded choice alternatives (root bag, Pathways, wrapped Curve list): card-1.
                branch-opts (when unbounded-choice? {:as-choice-branch? true})
                child-els (vec
                           (map-indexed
                            (fn [i p]
                              (emit-child-particle! path name i p (or branch-opts {})))
                            child-particles))
                attr-els (map-indexed
                          (fn [i a]
                            (emit-attribute! store *typedefs schema schema-id path a
                                             (+ (count child-els) i)))
                          attrs)
                ;; Build outer shape
                choice-item (when (and unbounded-choice? (not wrap-sole-choice?))
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
                wrapped-list
                (when wrap-sole-choice?
                  (let [frag (register-complex!
                              store
                              {:schema-id schema-id
                               :name (str name "_Fragment")
                               :documentation "Ordered heterogeneous choice item"
                               :kind :choice
                               :min-occurs 1
                               :max-occurs 1
                               :child-refs (mapv :record/id (remove nil? child-els))
                               :order 0})]
                    (register-complex!
                     store
                     {:schema-id schema-id
                      :name (str name "_list")
                      :documentation nil
                      :kind :collection
                      :min-occurs (or (:min-occurs only) 0)
                      :max-occurs (collection-max-occurs only)
                      :child-refs [(:record/id frag)]
                      :item-ref (:record/id frag)
                      :order 0})))
                outer-as-collection? (and unbounded-choice? (not wrap-sole-choice?))
                kind (cond
                       wrap-sole-choice? :sequence
                       outer-as-collection? :collection
                       choice? :choice
                       :else :sequence)
                child-refs (cond
                             outer-as-collection?
                             (into [(:record/id choice-item)] (mapv :record/id attr-els))
                             wrap-sole-choice?
                             (into [(:record/id wrapped-list)] (mapv :record/id attr-els))
                             :else
                             (into (mapv :record/id (remove nil? child-els))
                                   (mapv :record/id attr-els)))]
            (register-geometry-or-complex!
             store
             {:host-event-owner? as-host?
              :host-action-base host-action
              :schema-id schema-id
              :name name
              :documentation (or documentation (:documentation ct))
              :kind kind
              :min-occurs (if outer-as-collection?
                            (or (:min-occurs only) 0)
                            (or min-occurs 0))
              :max-occurs (if outer-as-collection?
                            (collection-max-occurs only)
                            (or max-occurs 1))
              :child-refs child-refs
              :item-ref (when outer-as-collection? (:record/id choice-item))
              :nillable? nillable?
              :order order
              :path path})))))))

(defn- emit-element-decl-body!
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
      (let [ict (:inline-complex el-decl)]
        (if (= :simple-content (:kind ict))
          (emit-simple-content! store *typedefs schema schema-id path name doc ict order)
          (emit-complex-type! store *typedefs schema schema-id path name doc
                              ict order
                              :min-occurs min-o :max-occurs max-o :nillable? nillable?)))

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
          (let [ct (get-in schema [:complex-types local])]
            (if (= :simple-content (:kind ct))
              (emit-simple-content! store *typedefs schema schema-id path name
                                    (or doc (:documentation ct)) ct order)
              (emit-complex-type! store *typedefs schema schema-id path name doc
                                  tref order
                                  :min-occurs min-o
                                  :max-occurs max-o
                                  :nillable? nillable?
                                  :host-event-owner? (geometry-host-event? name tref))))

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

(defn emit-element-decl!
  [store *typedefs schema schema-id path el-decl order]
  (let [el-decl (resolve-element-decl schema el-decl)
        name (:name el-decl)
        min-o (:min-occurs el-decl)
        max-o (:max-occurs el-decl)
        nillable? (:nillable? el-decl)]
    (when-not name
      (throw (ex-info "Element without name after ref resolution"
                      {:element el-decl :path path})))
    (let [type-key (decl-type-key el-decl)
          frame-key (emit-frame-key name min-o max-o nillable? type-key)]
      (if-let [frame (get *emit-frames* frame-key)]
        (do
          (reset! (:hit? frame) true)
          {:record/id (:id frame)})
        (let [frame {:name name
                     :type-key type-key
                     :id (u/stable-uuid (str "element/frame/" (pr-str frame-key)))
                     :hit? (atom false)}]
          (binding [*emit-frames* (assoc *emit-frames* frame-key frame)
                    *current-emit-frame* frame]
            (emit-element-decl-body! store *typedefs schema schema-id path el-decl order)))))))
