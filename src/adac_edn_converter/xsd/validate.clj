(ns adac-edn-converter.xsd.validate
  "Validate ADAC Instance XML against XSD (JDK javax.xml.validation)."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.io StringReader]
           [javax.xml XMLConstants]
           [javax.xml.transform.stream StreamSource]
           [javax.xml.validation SchemaFactory]
           [org.xml.sax ErrorHandler SAXParseException]))

(def ^:private max-errors 200)

(defn- stream-source-file
  "StreamSource with systemId so relative xs:include/import resolve."
  [path]
  (let [f (io/file path)
        uri (.toString (.toURI f))]
    (StreamSource. (io/input-stream f) uri)))

(defn- stream-source-xml
  "XML StreamSource from path, string, bytes, File, or InputStream/Reader."
  [xml-source]
  (cond
    (nil? xml-source)
    (throw (ex-info "XML source is required" {}))

    (string? xml-source)
    (let [s (str/trim xml-source)]
      (if (or (str/starts-with? s "<") (str/starts-with? s "<?"))
        (StreamSource. (StringReader. xml-source))
        (stream-source-file xml-source)))

    (bytes? xml-source)
    (StreamSource. (java.io.ByteArrayInputStream. ^bytes xml-source))

    (instance? java.io.File xml-source)
    (stream-source-file xml-source)

    (instance? java.io.InputStream xml-source)
    (StreamSource. ^java.io.InputStream xml-source)

    (instance? java.io.Reader xml-source)
    (StreamSource. ^java.io.Reader xml-source)

    :else
    (stream-source-file (str xml-source))))

(defn- parse-exception->error
  [severity ^SAXParseException e]
  (cond-> {:severity severity
           :message (or (.getMessage e) (str e))
           :line (.getLineNumber e)
           :column (.getColumnNumber e)}
    (not-empty (.getSystemId e))
    (assoc :system-id (.getSystemId e))))

(defn- collecting-error-handler
  "ErrorHandler that records issues and does not rethrow (continues validation)."
  [*errors]
  (reify ErrorHandler
    (warning [_ e]
      (when (< (count @*errors) max-errors)
        (swap! *errors conj (parse-exception->error :warning e))))
    (error [_ e]
      (when (< (count @*errors) max-errors)
        (swap! *errors conj (parse-exception->error :error e))))
    (fatalError [_ e]
      (when (< (count @*errors) max-errors)
        (swap! *errors conj (parse-exception->error :fatal e))))))

(defn validate-report
  "Validate XML against XSD and collect all handler messages.

  Returns {:valid? bool :errors [{:severity :line :column :message :system-id?}…]}.
  Does not throw for schema violations. Caps at 200 errors."
  [xsd-path xml-source]
  (let [*errors (atom [])
        factory (SchemaFactory/newInstance XMLConstants/W3C_XML_SCHEMA_NS_URI)
        schema (.newSchema factory (stream-source-file xsd-path))
        validator (.newValidator schema)]
    (.setErrorHandler validator (collecting-error-handler *errors))
    (try
      (.validate validator (stream-source-xml xml-source))
      (catch Exception e
        ;; Fatal parse failures may still throw after the handler records them.
        (when (empty? @*errors)
          (swap! *errors conj {:severity :fatal
                               :message (or (.getMessage e) (str e))
                               :line -1
                               :column -1}))))
    (let [errors (vec @*errors)
          hard? (some #(contains? #{:error :fatal} (:severity %)) errors)]
      {:valid? (not hard?)
       :errors errors})))

(defn validate!
  "Validate xml-path against xsd-path. Returns nil on success; throws on failure."
  [xsd-path xml-path]
  (let [report (validate-report xsd-path xml-path)]
    (if (:valid? report)
      nil
      (throw (ex-info "XML validation failed"
                      {:error (or (:message (first (:errors report)))
                                  "validation failed")
                       :errors (:errors report)})))))

(defn valid?
  [xsd-path xml-path]
  (:valid? (validate-report xsd-path xml-path)))

(defn validation-error
  "Validate and return first error message string, or nil if valid."
  [xsd-path xml-path]
  (let [report (validate-report xsd-path xml-path)]
    (when-not (:valid? report)
      (or (:message (first (:errors report)))
          "XML validation failed"))))
