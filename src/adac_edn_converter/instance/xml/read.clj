(ns adac-edn-converter.instance.xml.read
  "ADAC Instance XML → assembled SchemaCraft Instance EDN (value map).

  Lenient import policy (SchemaCraft validates later):
  - Requires well-formed XML only — does not run XSD validation.
  - Maps into known Element keys from the schema bundle.
  - Missing required fields → omit key (do not throw).
  - Bad scalar types / enums → keep raw string via coerce.
  - xsi:nil → :schemacraft/nil.
  - Unknown elements and attributes are dropped.
  - Partial choice / collection / Geometry content is kept as far as it maps.
  - Per-child errors are swallowed so one bad branch does not abort the file."
  (:require [adac-edn-converter.instance.coerce :as coerce]
            [adac-edn-converter.instance.schema-index :as idx]
            [adac-edn-converter.instance.xml-util :as xu]
            [clojure.java.io :as io]))

(def ^:private nil-sentinel :schemacraft/nil)

(declare read-complex)
(declare read-complex-body)

(defn- first-child-named
  [node name]
  (first (get (xu/children-by-name node) name)))

(defn- safe-part
  "Run body; on any exception return nil so import continues."
  [f]
  (try
    (f)
    (catch Exception _ nil)))

(defn- content-scalar?
  "True when child is the synthetic *_content scalar of a simpleContent Element."
  [parent-el child-el]
  (and parent-el child-el
       (idx/scalar? child-el)
       (= (str (name (:record/name parent-el)) "_content")
          (name (:record/name child-el)))))

(defn- simple-content-element?
  "Sequence that is only simple content value + optional parent-property attrs."
  [idx el]
  (let [kids (idx/children idx el)]
    (boolean
     (and (seq kids)
          (some #(content-scalar? el %) kids)
          (every? #(or (content-scalar? el %) (idx/parent-property? %)) kids)))))

(defn- read-scalar-value
  [idx el node]
  (let [td (idx/typedef-for-element idx el)]
    (cond
      (xu/xsi-nil? node) nil-sentinel
      :else (coerce/coerce-value (xu/text-content node) td))))

(defn- read-scalar
  [idx el node]
  (let [k (idx/element-key el)
        td (idx/typedef-for-element idx el)
        v (cond
            node (read-scalar-value idx el node)
            (idx/fixed-value el)
            (coerce/coerce-value (idx/fixed-value el) td)
            (and (>= (idx/min-occurs el) 1) (idx/nillable? el))
            nil-sentinel
            :else nil)]
    (when (or (some? v) (>= (idx/min-occurs el) 1))
      {k v})))

(defn- read-parent-property
  [idx el attrs]
  (let [k (idx/element-key el)
        prop (idx/property-name el)
        raw (or (get attrs prop)
                (idx/fixed-value el))
        td (idx/typedef-for-element idx el)
        v (when raw (coerce/coerce-value raw td))]
    (when (some? v)
      {k v})))

(defn- read-choice-branch
  "Read one choice alternative node into a choice value map (no wrapper)."
  [idx choice-el node]
  (let [alts (idx/children idx choice-el)
        local (xu/tag-local (:tag node))
        alt (some #(when (= local (name (:record/name %))) %) alts)]
    (when alt
      {(idx/element-key alt) (read-complex idx alt node)})))

(defn- read-choice
  "Read a choice Element wrapper node (e.g. ChamberSize containing Circular)."
  [idx el node]
  (let [k (idx/element-key el)
        alts (idx/children idx el)
        by-name (xu/children-by-name node)
        chosen (some (fn [alt]
                       (when-let [nodes (get by-name (name (:record/name alt)))]
                         [alt (first nodes)]))
                     alts)]
    (when chosen
      (let [[alt alt-node] chosen]
        {k {(idx/element-key alt) (read-complex idx alt alt-node)}}))))

(defn- read-transparent-choice-collection
  "Collection whose item is a choice: XML children are the alternatives directly
  (e.g. Path/Ring with PolySegment|Curve* — no Path_Fragment wrapper)."
  [idx coll-el item-el wrapper]
  (let [k (idx/element-key coll-el)
        alts (idx/children idx item-el)
        alt-names (into #{} (map #(name (:record/name %)) alts))
        nodes (->> (xu/element-children wrapper)
                   (filter #(contains? alt-names (xu/tag-local (:tag %)))))]
    {k (mapv (fn [node]
               (or (safe-part (fn [] (read-choice-branch idx item-el node)))
                   {}))
             nodes)}))

(defn- read-host-event
  [idx el node]
  (let [k (idx/element-key el)
        mode (or (idx/host-payload-mode el) :target)]
    (if (= mode :self)
      (when-let [body (read-complex-body idx el node)]
        {k body})
      (let [target (idx/host-target idx el)
            target-name (when target (name (:record/name target)))
            target-node (when target-name (first-child-named node target-name))]
        (when (and target target-node)
          {k {(idx/element-key target)
              (read-complex idx target target-node)}})))))

(defn- read-collection
  [idx child-el child-map]
  (let [k (idx/element-key child-el)
        el-name (name (:record/name child-el))
        item-el (idx/collection-item idx child-el)
        wrapper (first (get child-map el-name))]
    (cond
      (nil? wrapper) nil
      (xu/xsi-nil? wrapper) {k nil-sentinel}
      (and item-el (idx/choice? item-el))
      (read-transparent-choice-collection idx child-el item-el wrapper)
      :else
      (let [item-name (name (:record/name item-el))
            items (get (xu/children-by-name wrapper) item-name [])]
        {k (mapv #(or (safe-part (fn [] (read-complex idx item-el %)))
                      {})
                 items)}))))

(defn- with-required-nillables
  "Ensure required nillable children are present as :schemacraft/nil when omitted.
  Keeps assembled maps XSD-complete so export can validate."
  [idx el body]
  (let [body (if (map? body) body {})]
    (reduce
     (fn [acc child]
       (let [k (idx/element-key child)
             required-nillable?
             (and (>= (idx/min-occurs child) 1)
                  (idx/nillable? child)
                  (or (idx/scalar? child)
                      (idx/collection? child)
                      (idx/sequence? child)
                      (idx/choice? child)
                      (idx/host-event? child)))]
         (cond
           (and required-nillable? (not (contains? acc k)))
           (assoc acc k nil-sentinel)

           (and required-nillable? (nil? (get acc k)))
           (assoc acc k nil-sentinel)

           :else acc)))
     body
     (idx/children idx el))))

(defn- read-complex-body
  [idx el node]
  (let [attrs (xu/attrs-by-local node)
        child-map (xu/children-by-name node)
        parts (for [child-el (idx/children idx el)
                    :let [el-name (name (:record/name child-el))]]
                (safe-part
                 (fn []
                   (cond
                     (idx/parent-property? child-el)
                     (read-parent-property idx child-el attrs)

                     (content-scalar? el child-el)
                     (let [k (idx/element-key child-el)
                           td (idx/typedef-for-element idx child-el)
                           v (if (xu/xsi-nil? node)
                               nil-sentinel
                               (coerce/coerce-value (xu/text-content node) td))]
                       (when (some? v) {k v}))

                     (idx/scalar? child-el)
                     (read-scalar idx child-el (first (get child-map el-name)))

                     (idx/choice? child-el)
                     (when-let [cn (first (get child-map el-name))]
                       (read-choice idx child-el cn))

                     (idx/collection? child-el)
                     (read-collection idx child-el child-map)

                     (idx/host-event? child-el)
                     (when-let [cn (first (get child-map el-name))]
                       (read-host-event idx child-el cn))

                     :else
                     (when-let [cn (first (get child-map el-name))]
                       (let [k (idx/element-key child-el)
                             v (read-complex idx child-el cn)]
                         (when v {k v})))))))]
    (with-required-nillables idx el (apply merge (remove nil? parts)))))

(defn read-complex
  "Convert XML node → assembled value for Element el.

  Collection Elements treat `node` as the list wrapper and return a vector of
  items (e.g. PolySegment → [vertex-map …])."
  [idx el node]
  (when node
    (cond
      (xu/xsi-nil? node) nil-sentinel

      (idx/choice? el)
      (let [m (read-choice idx el node)]
        (get m (idx/element-key el)))

      (idx/collection? el)
      (let [item-el (idx/collection-item idx el)]
        (cond
          (nil? item-el) []
          (idx/choice? item-el)
          (get (read-transparent-choice-collection idx el item-el node)
               (idx/element-key el))
          :else
          (let [item-name (name (:record/name item-el))
                items (get (xu/children-by-name node) item-name [])]
            (mapv #(or (safe-part (fn [] (read-complex idx item-el %))) {})
                  items))))

      :else (read-complex-body idx el node))))

(defn xml-node->instance
  "Convert parsed ADAC XML root node → assembled Instance map (ADAC body)."
  [idx root-node]
  (let [root-el (idx/root-element idx)]
    (or (read-complex idx root-el root-node) {})))

(defn read-file
  "Read schema bundle + well-formed XML → assembled Instance EDN.
  Does not XSD-validate; unknown tags are ignored."
  [bundle xml-source]
  (let [idx (idx/build-index bundle)
        root (xu/parse-xml xml-source)]
    (xml-node->instance idx root)))

(defn read-file-path
  [bundle xml-path]
  (read-file bundle (io/input-stream xml-path)))
