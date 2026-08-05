(ns adac-edn-converter.instance.xml.write
  "Assembled SchemaCraft Instance EDN → ADAC Instance XML.

  Export for authority submission: emits known schema structure from the
  assembled value map. Presence rules:
  - Emit a child only when its key is present in the value map (or parent-property).
  - `:schemacraft/nil` → `xsi:nil=\"true\"`.
  - Empty collections `[]` → empty plural wrapper.
  - Do not invent `xsi:nil` or empty complexes for missing optional keys.
  - Collection items that are choices emit the selected alternative directly
    (no synthetic *_Fragment wrapper), matching ADAC Path/Ring XML."
  (:require [adac-edn-converter.instance.coerce :as coerce]
            [adac-edn-converter.instance.schema-index :as idx]
            [adac-edn-converter.instance.xml-util :as xu]
            [clojure.data.xml :as xml]
            [clojure.java.io :as io]))

(def ^:private nil-sentinel :schemacraft/nil)

(declare write-complex-content)
(declare write-collection)

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

(defn- content-scalar?
  [parent-el child-el]
  (and parent-el child-el
       (idx/scalar? child-el)
       (= (str (name (:record/name parent-el)) "_content")
          (name (:record/name child-el)))))

(defn- simple-content-element?
  [idx el]
  (let [kids (idx/children idx el)]
    (boolean
     (and (seq kids)
          (some #(content-scalar? el %) kids)
          (every? #(or (content-scalar? el %) (idx/parent-property? %)) kids)))))

(defn- write-simple-content
  "Emit simpleContent as element text + attribute properties (e.g. valname)."
  [idx el data]
  (let [el-name (name (:record/name el))
        kids (idx/children idx el)
        content-el (first (filter #(content-scalar? el %) kids))
        attrs (into {}
                    (mapcat #(parent-property-attrs idx % data)
                            (filter idx/parent-property? kids)))
        raw (when content-el (get data (idx/element-key content-el)))
        td (when content-el (idx/typedef-for-element idx content-el))]
    (cond
      (= raw nil-sentinel)
      (xml-node (xml-tag el-name) (merge attrs (xu/xsi-nil-attrs)) [])

      (and content-el (some? raw))
      (xml-node (xml-tag el-name) attrs [(coerce/format-value raw td)])

      (seq attrs)
      (xml-node (xml-tag el-name) attrs [])

      :else nil)))

(defn- write-choice-branch
  "Emit the selected choice alternative element (no choice wrapper)."
  [idx choice-el alt-data]
  (when (map? alt-data)
    (some (fn [alt]
            (let [k (idx/element-key alt)]
              (when (contains? alt-data k)
                (let [v (get alt-data k)]
                  (cond
                    (idx/collection? alt)
                    (write-collection idx alt v)

                    (simple-content-element? idx alt)
                    (write-simple-content idx alt (if (map? v) v {}))

                    :else
                    (xml-node (xml-tag (name (:record/name alt)))
                              {}
                              (write-complex-content idx alt v)))))))
          (idx/children idx choice-el))))

(defn- write-choice
  [idx el alt-data]
  (when-let [branch (write-choice-branch idx el alt-data)]
    (xml-node (xml-tag (name (:record/name el)))
              {}
              [branch])))

(defn- write-host-event
  [idx el data]
  (let [el-name (name (:record/name el))
        mode (or (idx/host-payload-mode el) :target)
        payload (get data (idx/element-key el))]
    (cond
      (= payload nil-sentinel)
      (xml-node (xml-tag el-name) (xu/xsi-nil-attrs) [])

      (and payload (map? payload))
      (if (= mode :self)
        (let [attrs (into {}
                          (mapcat #(parent-property-attrs idx % payload)
                                  (filter idx/parent-property? (idx/children idx el))))]
          (xml-node (xml-tag el-name)
                    attrs
                    (write-complex-content idx el payload)))
        (let [target (idx/host-target idx el)
              target-key (when target (idx/element-key target))
              target-data (if (and target-key (contains? payload target-key))
                            (get payload target-key)
                            payload)]
          (when (and target (some? target-data) (not= target-data nil-sentinel))
            (xml-node (xml-tag el-name)
                      {}
                      [(cond
                         (idx/collection? target)
                         (write-collection idx target target-data)

                         (simple-content-element? idx target)
                         (write-simple-content idx target (if (map? target-data) target-data {}))

                         :else
                         (xml-node (xml-tag (name (:record/name target)))
                                   {}
                                   (write-complex-content idx target target-data)))])))))))

(defn- write-collection
  [idx child-el v]
  (let [coll-name (name (:record/name child-el))
        item-el (idx/collection-item idx child-el)]
    (cond
      (= v nil-sentinel)
      (xml-node (xml-tag coll-name) (xu/xsi-nil-attrs) [])

      (and item-el (idx/choice? item-el))
      (xml-node (xml-tag coll-name)
                {}
                (map (fn [item]
                       (write-choice-branch idx item-el item))
                     (or (when (sequential? v) v) [])))

      (and item-el (idx/collection? item-el))
      ;; Nested collection item (e.g. Polyline → Path → fragments): each item value
      ;; is the inner collection's assembled vector; emit one wrapper per item.
      (xml-node (xml-tag coll-name)
                {}
                (map (fn [item]
                       (write-collection idx item-el item))
                     (or (when (sequential? v) v) [])))

      :else
      (let [item-name (name (:record/name item-el))]
        (xml-node (xml-tag coll-name)
                  {}
                  (map (fn [item]
                         (if (simple-content-element? idx item-el)
                           (write-simple-content idx item-el (if (map? item) item {}))
                           (xml-node (xml-tag item-name)
                                     {}
                                     (write-complex-content idx item-el item))))
                       (or (when (sequential? v) v) [])))))))

(defn- write-complex-content
  [idx el data]
  (when (map? data)
    (remove
     nil?
     (for [child-el (idx/children idx el)
           :let [k (idx/element-key child-el)
                 present? (contains? data k)
                 v (get data k)
                 required? (>= (idx/min-occurs child-el) 1)]
           :when (or present?
                     (idx/parent-property? child-el)
                     ;; ADAC domain bags: emit every collection wrapper under a present parent.
                     (idx/collection? child-el)
                     (and required? (idx/nillable? child-el))
                     ;; Required domain sequences (Transport, …) even when fixture omits them.
                     (and required? (idx/sequence? child-el)))]
       (cond
         (idx/parent-property? child-el) nil

         (idx/scalar? child-el)
         (cond
           present?
           (cond
             (= v nil-sentinel) (scalar-node idx child-el v)
             (nil? v) nil
             :else (scalar-node idx child-el v))
           (and required? (idx/nillable? child-el))
           (scalar-node idx child-el nil-sentinel)
           :else nil)

         (idx/choice? child-el)
         (when (and present? v (not= v nil-sentinel))
           (write-choice idx child-el v))

         (idx/collection? child-el)
         (cond
           present? (write-collection idx child-el v)
           (and required? (idx/nillable? child-el))
           (write-collection idx child-el nil-sentinel)
           :else (write-collection idx child-el []))

         (idx/host-event? child-el)
         (cond
           present? (write-host-event idx child-el data)
           (and required? (idx/nillable? child-el))
           (xml-node (xml-tag (name (:record/name child-el))) (xu/xsi-nil-attrs) [])
           :else nil)

         (= v nil-sentinel)
         (xml-node (xml-tag (name (:record/name child-el)))
                   (xu/xsi-nil-attrs)
                   [])

         (and present? (simple-content-element? idx child-el) (map? v))
         (write-simple-content idx child-el v)

         (and (map? v) (seq v))
         (xml-node (xml-tag (name (:record/name child-el)))
                   {}
                   (write-complex-content idx child-el v))

         (and (not present?) required? (idx/nillable? child-el))
         (xml-node (xml-tag (name (:record/name child-el)))
                   (xu/xsi-nil-attrs)
                   [])

         (and (not present?) required? (idx/sequence? child-el))
         (xml-node (xml-tag (name (:record/name child-el)))
                   {}
                   (write-complex-content idx child-el {}))

         :else nil)))))

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
