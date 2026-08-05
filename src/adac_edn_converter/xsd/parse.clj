(ns adac-edn-converter.xsd.parse
  "Parse ADAC / LandXML XSD into an indexable IR (follows xs:include)."
  (:require [clojure.data.xml :as xml]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [adac-edn-converter.util :as u]))

(defn- tag-local
  [el]
  (when el
    (u/local-name (:tag el))))

(defn- children
  [el]
  (filter map? (:content el)))

(defn- find-child
  [el local]
  (first (filter #(= (tag-local %) local) (children el))))

(defn- find-children
  [el local]
  (filter #(= (tag-local %) local) (children el)))

(defn- attrs
  [el]
  (:attrs el))

(defn- documentation
  "xs:documentation texts under xs:annotation; multiple siblings joined with newlines."
  [el]
  (when-let [ann (find-child el "annotation")]
    (let [docs (find-children ann "documentation")
          texts (keep (fn [d]
                        (let [raw (->> (:content d)
                                       (filter string?)
                                       (str/join)
                                       str/trim)
                              t (when-not (str/blank? raw)
                                  (str/replace raw #"\s+" " "))]
                          (when-not (str/blank? t) t)))
                      docs)]
      (when (seq texts)
        (str/join "\n" texts)))))

(defn- parse-facets
  [restriction-el]
  (reduce
   (fn [m child]
     (let [t (tag-local child)
           v (get-in child [:attrs :value])]
       (case t
         "enumeration"
         (update m :enumerations (fnil conj [])
                 {:value v :documentation (documentation child)})
         "minLength" (assoc m :min-length (Long/parseLong v))
         "maxLength" (assoc m :max-length (Long/parseLong v))
         "length" (assoc m :length (Long/parseLong v))
         "minInclusive" (assoc m :min-inclusive v)
         "maxInclusive" (assoc m :max-inclusive v)
         "minExclusive" (assoc m :min-exclusive v)
         "maxExclusive" (assoc m :max-exclusive v)
         "totalDigits" (assoc m :total-digits (Long/parseLong v))
         "fractionDigits" (assoc m :fraction-digits (Long/parseLong v))
         "pattern" (assoc m :pattern v)
         "whiteSpace" (assoc m :white-space v)
         m)))
   {}
   (children restriction-el)))

(declare parse-particle parse-complex-type-body)

(defn- parse-element
  [el]
  (let [a (attrs el)
        name (:name a)
        ref (when-let [r (:ref a)] (u/local-name r))
        type-ref (:type a)
        min-o (u/parse-occurs (:minOccurs a))
        max-o (u/parse-occurs (:maxOccurs a))
        nillable? (= "true" (:nillable a))
        default (:default a)
        fixed (:fixed a)
        doc (documentation el)
        inline-ct (find-child el "complexType")
        inline-st (find-child el "simpleType")]
    (if ref
      (cond-> {:kind :element
               :ref ref
               :min-occurs min-o
               :max-occurs max-o
               :documentation doc}
        (= "true" (:nillable a)) (assoc :nillable? true)
        default (assoc :default default)
        fixed (assoc :fixed fixed))
      (cond-> {:kind :element
               :name name
               :type-ref type-ref
               :min-occurs min-o
               :max-occurs max-o
               :nillable? nillable?
               :documentation doc}
        default (assoc :default default)
        fixed (assoc :fixed fixed)
        inline-ct (assoc :inline-complex (parse-complex-type-body inline-ct))
        inline-st (assoc :inline-simple
                         (let [r (find-child inline-st "restriction")]
                           {:base (get-in r [:attrs :base])
                            :facets (when r (parse-facets r))}))))))

(defn- parse-attribute
  [el]
  (let [a (attrs el)]
    {:kind :attribute
     :name (:name a)
     :type-ref (:type a)
     :use (or (:use a) "optional")
     :fixed (:fixed a)
     :default (:default a)
     :documentation (documentation el)}))

(defn- parse-particle
  [el]
  (case (tag-local el)
    "element" (parse-element el)
    "attribute" (parse-attribute el)
    "sequence"
    {:kind :sequence
     :min-occurs (u/parse-occurs (get-in el [:attrs :minOccurs]))
     :max-occurs (u/parse-occurs (get-in el [:attrs :maxOccurs]))
     :particles (mapv parse-particle
                      (filter #(#{"element" "sequence" "choice" "group" "any"}
                                (tag-local %))
                              (children el)))}
    "choice"
    {:kind :choice
     :min-occurs (u/parse-occurs (get-in el [:attrs :minOccurs]))
     :max-occurs (u/parse-occurs (get-in el [:attrs :maxOccurs]))
     :particles (mapv parse-particle
                      (filter #(#{"element" "sequence" "choice" "group"}
                                (tag-local %))
                              (children el)))}
    "group"
    (if-let [ref (get-in el [:attrs :ref])]
      {:kind :group-ref
       :ref ref
       :min-occurs (u/parse-occurs (get-in el [:attrs :minOccurs]))
       :max-occurs (u/parse-occurs (get-in el [:attrs :maxOccurs]))}
      {:kind :group
       :name (get-in el [:attrs :name])
       :particles (mapv parse-particle
                        (filter #(#{"element" "sequence" "choice" "group"}
                                  (tag-local %))
                                (children el)))})
    "any" {:kind :any}
    nil))

(defn- content-model-children
  [el]
  (mapv parse-particle
        (filter #(#{"element" "sequence" "choice" "group" "attribute"}
                  (tag-local %))
                (children el))))

(defn parse-complex-type-body
  [el]
  (let [doc (documentation el)
        seq-el (find-child el "sequence")
        choice-el (find-child el "choice")
        cc (find-child el "complexContent")
        sc (find-child el "simpleContent")
        attrs (mapv parse-attribute (find-children el "attribute"))]
    (cond
      cc
      (let [ext (find-child cc "extension")
            rest (find-child cc "restriction")
            node (or ext rest)]
        {:kind :complex
         :documentation doc
         :base (get-in node [:attrs :base])
         :derivation (if ext :extension :restriction)
         :particles (when node (content-model-children node))
         :attributes (into attrs (mapv parse-attribute (find-children node "attribute")))})

      sc
      (let [ext (find-child sc "extension")
            rest (find-child sc "restriction")
            node (or ext rest)]
        {:kind :simple-content
         :documentation doc
         :base (get-in node [:attrs :base])
         :derivation (if ext :extension :restriction)
         :attributes (mapv parse-attribute (find-children node "attribute"))
         :facets (when rest (parse-facets rest))})

      seq-el
      {:kind :complex
       :documentation doc
       :particles [(parse-particle seq-el)]
       :attributes attrs}

      choice-el
      {:kind :complex
       :documentation doc
       :particles [(parse-particle choice-el)]
       :attributes attrs}

      :else
      {:kind :complex
       :documentation doc
       :particles []
       :attributes attrs})))

(defn- parse-simple-type
  [el]
  (let [name (get-in el [:attrs :name])
        doc (documentation el)
        restriction (find-child el "restriction")
        list-el (find-child el "list")
        union-el (find-child el "union")]
    (cond
      restriction
      {:kind :simple
       :name name
       :documentation doc
       :base (get-in restriction [:attrs :base])
       :facets (parse-facets restriction)}
      list-el
      {:kind :simple
       :name name
       :documentation doc
       :list-item-type (get-in list-el [:attrs :itemType])}
      union-el
      {:kind :simple
       :name name
       :documentation doc
       :union-members (str/split (or (get-in union-el [:attrs :memberTypes]) "") #"\s+")}
      :else
      {:kind :simple :name name :documentation doc})))

(defn- parse-named-group
  [el]
  (let [name (get-in el [:attrs :name])
        body (or (find-child el "sequence")
                 (find-child el "choice"))]
    {:kind :group
     :name name
     :documentation (documentation el)
     :particle (when body (parse-particle body))}))

(defn- schema-top-level
  [schema-el]
  (children schema-el))

(def ^:private empty-indexes
  {:simple-types {}
   :complex-types {}
   :groups {}
   :elements {}})

(defn- index-top-level
  "Build IR index maps from one schema element's top-level children."
  [tops]
  {:simple-types
   (into {}
         (comp (filter #(= "simpleType" (tag-local %)))
               (map parse-simple-type)
               (map (juxt :name identity)))
         tops)
   :complex-types
   (into {}
         (comp (filter #(= "complexType" (tag-local %)))
               (map (fn [el]
                      (let [name (get-in el [:attrs :name])
                            body (parse-complex-type-body el)]
                        [name (assoc body
                                     :name name
                                     :abstract? (= "true" (get-in el [:attrs :abstract])))]))))
         tops)
   :groups
   (into {}
         (comp (filter #(and (= "group" (tag-local %))
                             (get-in % [:attrs :name])))
               (map parse-named-group)
               (map (juxt :name identity)))
         tops)
   :elements
   (into {}
         (comp (filter #(= "element" (tag-local %)))
               (map parse-element)
               (map (juxt :name identity)))
         tops)})

(defn- merge-indexes
  "Merge IR indexes; overlay keys win on name collisions."
  [base overlay]
  (-> base
      (update :simple-types merge (:simple-types overlay))
      (update :complex-types merge (:complex-types overlay))
      (update :groups merge (:groups overlay))
      (update :elements merge (:elements overlay))))

(defn- source-key
  "Canonical identity for cycle detection across include graphs."
  [source]
  (cond
    (instance? java.net.URL source)
    (.toExternalForm source)

    :else
    (let [f (io/file source)]
      (try
        (.getCanonicalPath f)
        (catch Exception _
          (.getAbsolutePath f))))))

(defn- resolve-include-source
  "Resolve xs:include schemaLocation relative to the including schema."
  [parent-source schema-location]
  (let [loc (-> schema-location str str/trim (str/replace #"^\./+" ""))]
    (when-not (str/blank? loc)
      (cond
        (instance? java.net.URL parent-source)
        (java.net.URL. parent-source loc)

        :else
        (let [parent-file (io/file parent-source)
              dir (.getParentFile parent-file)]
          (when dir
            (io/file dir loc)))))))

(defn- parse-schema*
  "Parse one XSD and recursively merge same-TNS xs:include targets."
  [source visited]
  (let [key (source-key source)]
    (if (contains? visited key)
      empty-indexes
      (let [visited (conj visited key)
            root (xml/parse-str (slurp source))
            local (index-top-level (schema-top-level root))
            from-includes
            (reduce
             (fn [acc inc-el]
               (if-let [resolved (resolve-include-source
                                  source
                                  (get-in inc-el [:attrs :schemaLocation]))]
                 (merge-indexes acc (parse-schema* resolved visited))
                 acc))
             empty-indexes
             (find-children root "include"))]
        ;; Local components win over included duplicates.
        (merge-indexes from-includes local)))))

(defn parse-schema
  "Parse an XSD File/URL/path into IR index maps.

  Follows xs:include (same target namespace) relative to the including file.
  Entry-document :version / :documentation / :target-namespace are kept from
  the root source only."
  [source]
  (let [root (xml/parse-str (slurp source))
        indexes (parse-schema* source #{})]
    (assoc indexes
           :target-namespace (get-in root [:attrs :targetNamespace])
           :version (get-in root [:attrs :version])
           :documentation (documentation root))))

(defn parse-classpath-xsd
  [resource-path]
  (if-let [url (io/resource resource-path)]
    (parse-schema url)
    (throw (ex-info "XSD resource not found" {:resource resource-path}))))
