(ns adac-edn-converter.instance.xml.read
  "ADAC Instance XML → SchemaCraft Instance EDN."
  (:require [adac-edn-converter.instance.coerce :as coerce]
            [adac-edn-converter.instance.schema-index :as idx]
            [adac-edn-converter.instance.xml-util :as xu]
            [clojure.java.io :as io]))

(def ^:private nil-sentinel :schemacraft/nil)

(declare read-complex)

(defn- first-child-named
  [node name]
  (first (get (xu/children-by-name node) name)))

(defn- read-scalar-value
  [idx el node]
  (let [td (idx/typedef-for-element idx el)]
    (cond
      (xu/xsi-nil? node) nil-sentinel
      :else (coerce/coerce-value (xu/text-content node) td))))

(defn- read-scalar
  [idx el node]
  (let [k (idx/element-key el)
        v (if node
            (read-scalar-value idx el node)
            (when-let [fixed (idx/fixed-value el)]
              (coerce/coerce-value fixed (idx/typedef-for-element idx el))))]
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

(defn- read-choice
  [idx el node]
  (let [k (idx/element-key el)
        alts (idx/children idx el)
        by-name (xu/children-by-name node)
        chosen (some (fn [alt]
                       (when-let [nodes (get by-name (name (:record/name alt)))]
                         [alt (first nodes)]))
                     alts)]
    (when chosen
      {k {(idx/element-key (first chosen))
          (read-complex idx (first chosen) (second chosen))}})))

(defn- read-host-event
  [idx el node]
  (let [k (idx/element-key el)
        target (idx/host-target idx el)
        target-name (name (:record/name target))
        target-node (first-child-named node target-name)]
    (when target-node
      {k {(idx/element-key target)
          (read-complex idx target target-node)}})))

(defn- read-complex-body
  [idx el node]
  (let [attrs (xu/attrs-by-local node)
        child-map (xu/children-by-name node)
        parts (for [child-el (idx/children idx el)
                    :let [el-name (name (:record/name child-el))]]
                (cond
                  (idx/parent-property? child-el)
                  (read-parent-property idx child-el attrs)

                  (idx/scalar? child-el)
                  (read-scalar idx child-el (first (get child-map el-name)))

                  (idx/choice? child-el)
                  (when-let [cn (first (get child-map el-name))]
                    (read-choice idx child-el cn))

                  (idx/collection? child-el)
                  (let [k (idx/element-key child-el)
                        item-el (idx/collection-item idx child-el)
                        item-name (name (:record/name item-el))
                        wrapper (first (get child-map el-name))
                        items (if wrapper
                                (get (xu/children-by-name wrapper) item-name [])
                                (get child-map item-name []))]
                    (if (seq items)
                      {k (mapv #(read-complex idx item-el %) items)}
                      (when (>= (idx/min-occurs child-el) 1)
                        {k []})))

                  (idx/host-event? child-el)
                  (when-let [cn (first (get child-map el-name))]
                    (read-host-event idx child-el cn))

                  :else
                  (when-let [cn (first (get child-map el-name))]
                    (let [k (idx/element-key child-el)
                          v (read-complex idx child-el cn)]
                      (when v {k v})))))]
    (apply merge (remove nil? parts))))

(defn read-complex
  [idx el node]
  (when node
    (cond
      (xu/xsi-nil? node) nil-sentinel
      (idx/choice? el) (read-choice idx el node)
      :else (read-complex-body idx el node))))

(defn xml-node->instance
  "Convert parsed ADAC XML root node → Instance EDN map under :ADAC."
  [idx root-node]
  (let [root-el (idx/root-element idx)]
    (read-complex idx root-el root-node)))

(defn read-file
  "Read schema bundle + XML file → Instance EDN."
  [bundle xml-source]
  (let [idx (idx/build-index bundle)
        root (xu/parse-xml xml-source)]
    (xml-node->instance idx root)))

(defn read-file-path
  [bundle xml-path]
  (read-file bundle (io/input-stream xml-path)))
