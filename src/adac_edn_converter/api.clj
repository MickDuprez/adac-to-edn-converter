(ns adac-edn-converter.api
  "Library API: ADAC XML ↔ SchemaCraft instance-graph (import + export).

  LandXML stays CLI-only — not exposed here."
  (:require [adac-edn-converter.convert :as convert]
            [adac-edn-converter.edn.read :as edn-read]
            [adac-edn-converter.instance.graph :as graph]
            [adac-edn-converter.instance.schema-index :as idx]
            [adac-edn-converter.instance.xml.read :as xml-read]
            [adac-edn-converter.instance.xml.write :as xml-write]
            [adac-edn-converter.instance.xml-util :as xu]
            [adac-edn-converter.upgrade :as upgrade]
            [adac-edn-converter.xsd.validate :as xsd-validate]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def schema-resource-v600 "adac/edn/adac-v600.edn")
(def schema-resource-v501 "adac/edn/adac-v501.edn")

(def xsd-resource-v600 "adac/ADAC_V600_Flattened.xsd")
(def xsd-resource-v501 "adac/ADAC_v501_XSD/ADAC_V501.xsd")

(def schema-id-v600 convert/adac-schema-id-v600)
(def schema-id-v501 convert/adac-schema-id-v501)

(defn schema-id-for-version
  "Map an ADAC @version string to the converter schema UUID."
  [version]
  (convert/adac-schema-id-for version))

(defn- xml-input
  "Coerce upload/path/string sources to something clojure.data.xml can parse.
  Strings that look like XML are parsed as content; other strings as paths."
  [xml-source]
  (cond
    (nil? xml-source)
    (throw (ex-info "XML source is required" {}))

    (bytes? xml-source)
    (java.io.ByteArrayInputStream. ^bytes xml-source)

    (string? xml-source)
    (let [s (str/trim xml-source)]
      (if (or (str/starts-with? s "<") (str/starts-with? s "<?"))
        (java.io.StringReader. xml-source)
        (io/input-stream (io/file xml-source))))

    (instance? java.io.File xml-source)
    (io/input-stream xml-source)

    (instance? java.net.URL xml-source)
    (io/input-stream xml-source)

    (instance? java.net.URI xml-source)
    (io/input-stream xml-source)

    :else xml-source))

(defn peek-adac-version
  "Read root ADAC @version from XML (string, bytes, File, URL, InputStream, or Reader).
  Returns nil when absent or not well-formed."
  [xml-source]
  (try
    (let [root (xu/parse-xml (xml-input xml-source))
          attrs (xu/attrs-by-local root)]
      (not-empty (str/trim (str (or (get attrs "version") "")))))
    (catch Exception _ nil)))

(defn suggest-schema-id
  "Default target schema id from XML @version (5.x → v501, else v600)."
  [xml-source]
  (schema-id-for-version (or (peek-adac-version xml-source) "6.0.0")))

(defn load-schema-bundle
  "Load a packaged ADAC schema EDN. `which` is :v600, :v501, a version string,
  a schema UUID, a classpath resource path, or a readable File/URL."
  [which]
  (cond
    (map? which) which

    (or (= which :v600)
        (= which schema-id-v600)
        (and (string? which) (str/starts-with? (str/trim which) "6.")))
    (edn-read/read-edn (io/resource schema-resource-v600))

    (or (= which :v501)
        (= which schema-id-v501)
        (and (string? which) (str/starts-with? (str/trim which) "5.")))
    (edn-read/read-edn (io/resource schema-resource-v501))

    (string? which)
    (let [r (io/resource which)]
      (edn-read/read-edn (or r which)))

    :else
    (edn-read/read-edn which)))

(defn- v5-version?
  [version]
  (str/starts-with? (str/trim (str (or version ""))) "5."))

(defn- target-v600?
  [target-schema-id]
  (= target-schema-id schema-id-v600))

(defn- target-v501?
  [target-schema-id]
  (= target-schema-id schema-id-v501))

(defn- filename-stem
  [filename]
  (when-let [n (not-empty (str filename))]
    (let [base (last (str/split n #"[\\/]"))
          i (str/last-index-of base \.)]
      (if (and i (pos? i))
        (subs base 0 i)
        base))))

(defn xml->assembled
  "Parse ADAC XML with the given schema bundle → assembled body map."
  [bundle xml-source]
  (xml-read/read-file bundle (xml-input xml-source)))

(defn assembled->instance-graph
  [bundle assembled & {:keys [label]}]
  (graph/assembled->document-graph bundle assembled :label label))

(defn- materialize-xml
  "Buffer streams so peek + parse can both read the payload."
  [xml-source]
  (cond
    (or (bytes? xml-source)
        (string? xml-source)
        (instance? java.io.File xml-source)
        (instance? java.net.URL xml-source)
        (instance? java.net.URI xml-source))
    xml-source

    :else
    (let [baos (java.io.ByteArrayOutputStream.)]
      (io/copy (xml-input xml-source) baos)
      (.toByteArray baos))))

(defn convert-adac-xml
  "Convert ADAC XML into a SchemaCraft :document instance-graph.

  opts:
    :target-schema-id  — converter UUID (schema/ADAC-6.0.0 or …-5.0.1); required
    :label             — document label (default: filename stem / Project Name)
    :filename          — optional upload filename for default label
    :bundle-v600 / :bundle-v501 — optional preloaded schema EDN maps

  When XML is 5.x and target is v6, runs upgrade-v501->v600 then graphs as v6.
  Native paths: 5→v501, 6→v600. Mismatched 6→v501 is rejected."
  [xml-source {:keys [target-schema-id label filename bundle-v600 bundle-v501]
               :as opts}]
  (when-not (uuid? target-schema-id)
    (throw (ex-info "target-schema-id must be a UUID" {:opts opts})))
  (let [xml-source (materialize-xml xml-source)
        version (peek-adac-version xml-source)
        v5? (v5-version? version)
        label' (or (not-empty (str/trim (str (or label ""))))
                   (filename-stem filename))
        bundle-v600 (or bundle-v600 (delay (load-schema-bundle :v600)))
        bundle-v501 (or bundle-v501 (delay (load-schema-bundle :v501)))
        resolve (fn [b] (if (delay? b) @b b))]
    (cond
      (and v5? (target-v600? target-schema-id))
      (let [b5 (resolve bundle-v501)
            b6 (resolve bundle-v600)
            assembled (xml->assembled b5 xml-source)
            upgraded (upgrade/upgrade-v501->v600 b6 assembled)]
        {:valid? true
         :upgraded? true
         :xml-version version
         :target-schema-id target-schema-id
         :assembled upgraded
         :envelope (assembled->instance-graph b6 upgraded :label label')})

      (and v5? (target-v501? target-schema-id))
      (let [b5 (resolve bundle-v501)
            assembled (xml->assembled b5 xml-source)]
        {:valid? true
         :upgraded? false
         :xml-version version
         :target-schema-id target-schema-id
         :assembled assembled
         :envelope (assembled->instance-graph b5 assembled :label label')})

      (and (not v5?) (target-v600? target-schema-id))
      (let [b6 (resolve bundle-v600)
            assembled (xml->assembled b6 xml-source)]
        {:valid? true
         :upgraded? false
         :xml-version version
         :target-schema-id target-schema-id
         :assembled assembled
         :envelope (assembled->instance-graph b6 assembled :label label')})

      (and (not v5?) (target-v501? target-schema-id))
      {:valid? false
       :error "Cannot import ADAC 6.x XML into the ADAC 5.0.1 schema."
       :xml-version version
       :target-schema-id target-schema-id}

      :else
      {:valid? false
       :error (str "Unsupported target schema id: " target-schema-id)
       :xml-version version
       :target-schema-id target-schema-id})))

(defn packaged-schema-edns
  "Return [{:id :name :edn} …] for both packaged ADAC schemas."
  []
  (let [v600 (load-schema-bundle :v600)
        v501 (load-schema-bundle :v501)]
    [{:id (get-in v600 [:schema :record/id])
      :name (or (get-in v600 [:schema :record/label])
                (get-in v600 [:schema :record/name])
                "ADAC 6.0.0")
      :edn v600}
     {:id (get-in v501 [:schema :record/id])
      :name (or (get-in v501 [:schema :record/label])
                (get-in v501 [:schema :record/name])
                "ADAC 5.0.1")
      :edn v501}]))

(defn- resource-file
  "Resolve a classpath resource to a java.io.File when possible (file: URLs).
  Returns nil when the resource is inside a jar (caller must copy)."
  [resource-path]
  (when-let [url (io/resource resource-path)]
    (when (= "file" (.getProtocol url))
      (io/file (.toURI url)))))

(defn- copy-resource-to-temp
  [resource-path]
  (let [url (io/resource resource-path)
        _ (when-not url
            (throw (ex-info "XSD resource not found" {:resource resource-path})))
        suffix (let [n (last (str/split resource-path #"/"))]
                 (if (str/includes? n ".")
                   (str "." (last (str/split n #"\.")))
                   ".xsd"))
        tmp (java.io.File/createTempFile "adac-xsd-" suffix)]
    (.deleteOnExit tmp)
    (with-open [in (io/input-stream url)
                out (io/output-stream tmp)]
      (io/copy in out))
    tmp))

(defn resolve-xsd-path
  "Filesystem path to the packaged XSD for :v600, :v501, a schema UUID, or version."
  [which]
  (let [resource (cond
                   (or (= which :v600)
                       (= which schema-id-v600)
                       (and (string? which) (str/starts-with? (str/trim which) "6.")))
                   xsd-resource-v600

                   (or (= which :v501)
                       (= which schema-id-v501)
                       (and (string? which) (str/starts-with? (str/trim which) "5.")))
                   xsd-resource-v501

                   (string? which)
                   which

                   :else
                   xsd-resource-v600)
        f (or (resource-file resource)
              (when (and (string? which) (.exists (io/file which)))
                (io/file which))
              (copy-resource-to-temp resource))]
    (.getAbsolutePath ^java.io.File f)))

(defn- adac-body
  "Extract ADAC assembled body from Document map {:ADAC …} or bare body."
  [data]
  (cond
    (nil? data) {}
    (and (map? data) (contains? data :ADAC)) (:ADAC data)
    (and (map? data) (contains? data :ADACRoot)) (:ADACRoot data)
    :else data))

(defn export-adac-xml
  "Assembled map or instance-graph → XML string + soft XSD validation report.

  `which` is a schema bundle map, :v600/:v501, schema UUID, or version string.
  Does not enforce SchemaCraft deferred gates (host responsibility).

  Returns {:xml string :xsd-path string :valid? bool :errors […]}."
  [which data]
  (let [bundle (load-schema-bundle which)
        schema-id (get-in bundle [:schema :record/id])
        xsd-path (resolve-xsd-path (or schema-id which))
        assembled (-> (graph/coerce-assembled bundle data)
                      adac-body)
        ix (idx/build-index bundle)
        xml (xml-write/emit-str ix assembled)
        report (xsd-validate/validate-report xsd-path xml)]
    {:xml xml
     :xsd-path xsd-path
     :valid? (:valid? report)
     :errors (:errors report)}))
