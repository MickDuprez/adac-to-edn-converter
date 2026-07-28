(ns adac-edn-converter.xsd.validate
  "Validate ADAC Instance XML against Flattened XSD (JDK javax.xml.validation)."
  (:require [clojure.java.io :as io])
  (:import [javax.xml XMLConstants]
           [javax.xml.transform.stream StreamSource]
           [javax.xml.validation SchemaFactory]))

(defn validate!
  "Validate xml-path against xsd-path. Returns nil on success; throws on failure."
  [xsd-path xml-path]
  (let [factory (SchemaFactory/newInstance XMLConstants/W3C_XML_SCHEMA_NS_URI)
        schema (.newSchema factory (StreamSource. (io/input-stream xsd-path)))
        validator (.newValidator schema)]
    (.validate validator (StreamSource. (io/input-stream xml-path)))
    nil))

(defn valid?
  [xsd-path xml-path]
  (try
    (validate! xsd-path xml-path)
    true
    (catch Exception _ false)))

(defn validation-error
  "Validate and return error message string, or nil if valid."
  [xsd-path xml-path]
  (try
    (validate! xsd-path xml-path)
    nil
    (catch Exception e
      (.getMessage e))))
