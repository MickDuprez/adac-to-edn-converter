(ns adac-edn-converter.instance.xml.write
  "SchemaCraft Instance EDN → ADAC Instance XML."
  (:require [adac-edn-converter.instance.coerce :as coerce]
            [adac-edn-converter.instance.schema-index :as idx]
            [adac-edn-converter.instance.xml-util :as xu]
            [clojure.data.xml :as xml]
            [clojure.java.io :as io]))

(def ^:private nil-sentinel :schemacraft/nil)

(declare write-complex-content)

(defn- xml-tag [local]
  (xu/xml-tag-kw xu/adac-ns local))

(defn- xml-node
  ([tag-kw content]
   (xml-node tag-kw {} content))
  ([tag-kw attrs content]
   {:tag tag-kw
    :attrs (or attrs {})
    :content (vec (remove nil? (flatten content)))}))

(defn- scalar-node
  [idx el value]
  (let [el-name (name (:record/name el))
        td (idx/typedef-for-element idx el)]
    (cond
      (= nil-sentinel value)
      (xml-node (xml-tag el-name) (xu/xsi-nil-attrs) [])

      :else
      (xml-node (xml-tag el-name) {} [(coerce/format-value value td)]))))

(defn- parent-property-attrs
  [idx el data]
  (when-let [v (get data (idx/element-key el))]
    {(keyword (idx/property-name el))
     (coerce/format-value v (idx/typedef-for-element idx el))}))

(defn- write-choice
  [idx el alt-data]
  (some (fn [alt]
          (when-let [v (get alt-data (idx/element-key alt))]
            (xml-node (xml-tag (name (:record/name el)))
                      {}
                      [(xml-node (xml-tag (name (:record/name alt)))
                                 {}
                                 (write-complex-content idx alt v))])))
        (idx/children idx el)))

(defn- write-host-event
  [idx el data]
  (let [el-name (name (:record/name el))
        mode (or (idx/host-payload-mode el) :target)
        payload (get data (idx/element-key el))]
    (when (and payload (map? payload))
      (if (= mode :self)
        (let [attrs (into {}
                          (mapcat #(parent-property-attrs idx % payload)
                                  (filter idx/parent-property? (idx/children idx el))))]
          (xml-node (xml-tag el-name)
                    attrs
                    (write-complex-content idx el payload)))
        (let [target (idx/host-target idx el)
              target-key (idx/element-key target)
              target-data (get payload target-key payload)]
          (xml-node (xml-tag el-name)
                    {}
                    [(xml-node (xml-tag (name (:record/name target)))
                               {}
                               (write-complex-content idx target target-data))]))))))

(defn- write-complex-content
  [idx el data]
  (when (map? data)
    (remove
     nil?
     (for [child-el (idx/children idx el)
           :let [k (idx/element-key child-el)
                 v (get data k)]]
       (cond
         (idx/parent-property? child-el) nil

         (idx/scalar? child-el)
         (cond
           (= v nil-sentinel) (scalar-node idx child-el v)
           (nil? v) (when (idx/nillable? child-el)
                      (scalar-node idx child-el nil-sentinel))
           (contains? data k) (scalar-node idx child-el v))

         (idx/choice? child-el)
         (when (and v (not= v nil-sentinel)) (write-choice idx child-el v))

         (idx/collection? child-el)
         (let [coll-name (name (:record/name child-el))
               item-el (idx/collection-item idx child-el)
               item-name (name (:record/name item-el))
               present? (contains? data k)
               items (when present? v)]
           ;; Always emit the plural wrapper when writing a parent. List :min
           ;; controls item count (SchemaCraft), not whether the wrapper appears in XML.
           (if (or (= items nil-sentinel)
                   (and (not present?) (idx/nillable? child-el)))
             (xml-node (xml-tag coll-name) (xu/xsi-nil-attrs) [])
             (xml-node (xml-tag coll-name)
                       {}
                       (map (fn [item]
                              (xml-node (xml-tag item-name)
                                        {}
                                        (write-complex-content idx item-el item)))
                            (or (when (sequential? items) items) [])))))

         (idx/host-event? child-el)
         (write-host-event idx child-el data)

         (= v nil-sentinel)
         (xml-node (xml-tag (name (:record/name child-el)))
                   (xu/xsi-nil-attrs)
                   [])

         (and (map? v) (seq v))
         (xml-node (xml-tag (name (:record/name child-el)))
                   {}
                   (write-complex-content idx child-el v))

         (and (not (contains? data k))
              (>= (idx/min-occurs child-el) 1)
              (not (idx/scalar? child-el))
              (not (idx/collection? child-el))
              (not (idx/choice? child-el))
              (not (idx/host-event? child-el)))
         (xml-node (xml-tag (name (:record/name child-el)))
                   {}
                   (write-complex-content idx child-el {})))))))

(defn write-complex
  [idx el data]
  (let [name (name (:record/name el))
        attrs (into {}
                    (mapcat #(parent-property-attrs idx % data)
                            (filter idx/parent-property? (idx/children idx el))))
        content (write-complex-content idx el data)]
    (xml-node (xml-tag name) attrs content)))

(defn instance->xml-node
  "Convert Instance EDN (ADAC map) → XML element node."
  [idx instance]
  (let [root-el (idx/root-element idx)]
    (write-complex idx root-el instance)))

(defn emit-str
  [idx instance]
  (xml/emit-str (instance->xml-node idx instance)))

(defn write-file
  [bundle instance out-path]
  (let [idx (idx/build-index bundle)
        s (emit-str idx instance)]
    (spit out-path s)
    out-path))
