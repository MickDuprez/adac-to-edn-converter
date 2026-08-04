(ns adac-edn-converter.map.typedefs
  "Map XSD simpleTypes (and XS builtins) → SchemaCraft TypeDef records."
  (:require [adac-edn-converter.util :as u]
            [clojure.string :as str]))

(defn typedef-id
  [schema-id type-name]
  (u/stable-uuid (str "typedef/" (u/local-name type-name))))

(defn- resolve-base-chain
  "Follow restriction bases through user simpleTypes until builtin or missing."
  [simple-types type-name]
  (loop [n type-name seen #{}]
    (cond
      (nil? n) nil
      (seen n) n
      (u/xs-qname? n) n
      :else
      (if-let [st (get simple-types (u/local-name n))]
        (recur (:base st) (conj seen n))
        n))))

(defn- merge-facet-chain
  "Collect facets along the restriction chain (child overrides / adds)."
  [simple-types type-name]
  (loop [n type-name acc {} seen #{}]
    (cond
      (or (nil? n) (seen n) (u/xs-qname? n))
      acc
      :else
      (if-let [st (get simple-types (u/local-name n))]
        (recur (:base st)
               (merge (:facets st) acc)
               (conj seen n))
        acc))))

(defn- numeric-facet
  [v]
  (cond
    (nil? v) nil
    (number? v) v
    :else
    (try
      (Double/parseDouble (str v))
      (catch Exception _ v))))

(defn- facets->sc
  "XSD facets → SchemaCraft :typedef/facets map."
  [facets primitive]
  (cond-> {}
    (:max-length facets) (assoc :max-length (:max-length facets))
    (:min-length facets) (assoc :min-length (:min-length facets))
    (:length facets) (assoc :length (:length facets))
    (:fraction-digits facets) (assoc :fraction-digits (:fraction-digits facets))
    (:total-digits facets) (assoc :total-digits (:total-digits facets))
    (:pattern facets) (assoc :pattern (:pattern facets))
    (:min-inclusive facets) (assoc :min (numeric-facet (:min-inclusive facets)))
    (:max-inclusive facets) (assoc :max (numeric-facet (:max-inclusive facets)))
    (:min-exclusive facets) (assoc :min-exclusive (numeric-facet (:min-exclusive facets)))
    (:max-exclusive facets) (assoc :max-exclusive (numeric-facet (:max-exclusive facets)))
    (= primitive :string) (assoc :trim? true)))

(defn- list-item-along-chain
  "If this simpleType (or an ancestor) is xs:list, return itemType local name."
  [simple-types type-name]
  (loop [n type-name seen #{}]
    (cond
      (or (nil? n) (seen n) (u/xs-qname? n)) nil
      :else
      (let [st (get simple-types (u/local-name n))]
        (if-let [li (:list-item-type st)]
          (u/local-name li)
          (recur (:base st) (conj seen n)))))))

(defn classify-simple
  "Return {:primitive … :facets …} for a named or builtin simple type."
  [simple-types type-name]
  (let [local (u/local-name type-name)]
    (if (u/xs-qname? type-name)
      (let [prim (u/builtin-primitive type-name)
            facets (cond-> {}
                     (= prim :string) (assoc :trim? true)
                     (= local "positiveInteger") (assoc :min 1)
                     (= local "nonNegativeInteger") (assoc :min 0))]
        {:primitive prim :facets facets})
      (let [st (get simple-types local)
            list-item (or (:list-item-type st)
                          (list-item-along-chain simple-types local))
            facets (merge-facet-chain simple-types local)
            enums (:enumerations facets)
            base (resolve-base-chain simple-types local)]
        (if list-item
          {:primitive :string
           :facets (cond-> (facets->sc (dissoc facets :enumerations) :string)
                     true (assoc :trim? true
                                 :list? true
                                 :item-type (u/local-name list-item)))
           :documentation (:documentation st)
           :xsd-name local}
          (let [prim (if (seq enums)
                       :enum
                       (u/builtin-primitive (or base "string")))
                sc-facets (if (= prim :enum)
                            {:base :string
                             :items (mapv :value enums)}
                            (facets->sc (dissoc facets :enumerations) prim))]
            {:primitive prim
             :facets sc-facets
             :documentation (:documentation st)
             :xsd-name local}))))))

(defn emit-typedef
  [schema-id simple-types type-name]
  (let [local (u/local-name type-name)
        id (typedef-id schema-id local)
        {:keys [primitive facets documentation]} (classify-simple simple-types local)
        label (u/element-label local)
        kw (u/as-keyword local)]
    {:record/type :typedef
     :record/id id
     :schema/id schema-id
     :record/parent-id schema-id
     :record/name kw
     :record/label label
     :record/documentation documentation
     :typedef/primitive primitive
     :typedef/facets facets
     :element/key kw
     :element/kind :typedef
     :element/label label
     :element/documentation documentation
     :element/cardinality {:min 0 :max 1}
     :element/children []
     :element/data {:type/primitive primitive
                    :type/facets facets}}))

(defn emit-builtin-typedef
  [schema-id type-name]
  (emit-typedef schema-id {} type-name))

(defn string-type-names
  [simple-types]
  (->> (keys simple-types)
       (filter #(str/starts-with? % "String_"))
       sort))

(defn all-simple-type-names
  [simple-types]
  (sort (keys simple-types)))
