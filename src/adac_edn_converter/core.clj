(ns adac-edn-converter.core
  "CLI: ADAC/LandXML XSD ↔ SchemaCraft EDN ↔ Instance XML; BCIB legacy → EDN"
  (:require [adac-edn-converter.bcib.convert :as bcib]
            [adac-edn-converter.convert :as convert]
            [adac-edn-converter.edn.emit :as emit]
            [adac-edn-converter.edn.read :as edn-read]
            [adac-edn-converter.instance.xml.read :as xml-read]
            [adac-edn-converter.instance.xml.write :as xml-write]
            [adac-edn-converter.xsd.validate :as validate]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:gen-class))

(def default-xsd "adac/ADAC_V600_Flattened.xsd")
(def default-landxml-xsd "landxml/LandXML-1.2.xsd")
(def default-sample-xml "adac/Sample-ADAC-V6.0.0.xml")
(def default-bcib-legacy "doc/bcib/bcib-schema.edn")
(def default-bcib-overlay "doc/bcib/behaviour-overlay.csv")

(defn- parse-args
  [args]
  (loop [args args
         opts {:slice :sewerage-mh :profile :adac}]
    (cond
      (empty? args) opts
      (= "--full" (first args)) (recur (rest args) (assoc opts :slice nil))
      (= "--schema" (first args))
      (let [v (second args)
            profile (case v
                      ("adac" "ADAC") :adac
                      ("landxml" "LandXML" "landxml-1.2") :landxml
                      ("bcib" "BCIB") :bcib
                      (keyword v))]
        (recur (drop 2 args)
               (cond-> (assoc opts :profile profile)
                 (#{:landxml :bcib} profile) (assoc :slice nil))))
      (= "--slice" (first args))
      (let [v (second args)
            slice (case v
                    "sewerage-mh" :sewerage-mh
                    "none" nil
                    (keyword v))]
        (recur (drop 2 args) (assoc opts :slice slice)))
      (nil? (:xsd opts))
      (recur (rest args) (assoc opts :xsd (first args)))
      (nil? (:out opts))
      (recur (rest args) (assoc opts :out (first args)))
      :else
      (recur (rest args) opts))))

(defn- resolve-path
  [path default-resource]
  (cond
    (nil? path)
    (or (io/resource default-resource)
        (io/file "resources" default-resource))
    (.exists (io/file path)) (io/file path)
    (io/resource path) (io/resource path)
    :else (io/file path)))

(defn convert-bcib!
  "Convert legacy BCIB EDN + overlay → SchemaCraft EDN."
  [{:keys [xsd out]}]
  (let [legacy-path (or (when xsd (str (resolve-path xsd nil)))
                        (when (.exists (io/file default-bcib-legacy))
                          default-bcib-legacy)
                        (throw (ex-info "BCIB legacy EDN not found"
                                        {:tried default-bcib-legacy})))
        overlay-path (if (.exists (io/file default-bcib-overlay))
                       default-bcib-overlay
                       default-bcib-overlay)
        out-path (or out "target/bcib.edn")
        _ (io/make-parents out-path)
        bundle (bcib/convert-files legacy-path overlay-path)]
    (emit/write-edn bundle out-path)
    {:out out-path
     :typedef-count (count (:typedefs bundle))
     :element-count (count (:elements bundle))
     :slice nil
     :profile :bcib}))

(defn convert!
  "Convert XSD (or BCIB legacy EDN) to SchemaCraft EDN file."
  [{:keys [xsd out slice profile] :or {slice :sewerage-mh profile :adac} :as opts}]
  (if (= profile :bcib)
    (convert-bcib! opts)
    (let [default (if (= profile :landxml) default-landxml-xsd default-xsd)
          src (resolve-path xsd default)
          out-path (or out
                       (case profile
                         :landxml "target/landxml-1.2.edn"
                         (if (= slice :sewerage-mh)
                           "target/adac-v600-sewerage-mh.edn"
                           "target/adac-v600.edn")))
          _ (io/make-parents out-path)
          bundle (convert/convert
                  (adac-edn-converter.xsd.parse/parse-schema src)
                  :slice slice
                  :profile profile)]
      (emit/write-edn bundle out-path)
      {:out out-path
       :typedef-count (count (:typedefs bundle))
       :element-count (count (:elements bundle))
       :slice slice
       :profile profile})))

(defn xml-to-edn!
  [{:keys [schema xml out]}]
  (let [schema-path (if schema
                      (resolve-path schema nil)
                      (io/file "target/adac-v600.edn"))
        xml-path (resolve-path xml default-sample-xml)
        out-path (or out "target/adac-instance.edn")
        bundle (edn-read/read-edn schema-path)
        instance (xml-read/read-file-path bundle (str xml-path))
        _ (io/make-parents out-path)]
    (emit/write-edn instance out-path)
    {:out out-path}))

(defn edn-to-xml!
  [{:keys [schema instance out]}]
  (let [schema-path (if schema
                      (resolve-path schema nil)
                      (io/file "target/adac-v600.edn"))
        instance-path (if instance
                        (resolve-path instance nil)
                        (io/file "target/adac-instance.edn"))
        out-path (or out "target/adac-roundtrip.xml")
        bundle (edn-read/read-edn schema-path)
        instance-data (edn-read/read-edn instance-path)
        _ (io/make-parents out-path)]
    (xml-write/write-file bundle instance-data out-path)
    {:out out-path}))

(defn validate-xml!
  [{:keys [xsd xml]}]
  (let [xsd-path (str (resolve-path xsd default-xsd))
        xml-path (str (resolve-path xml default-sample-xml))
        err (validate/validation-error xsd-path xml-path)]
    (if err
      (throw (ex-info "XML validation failed" {:error err}))
      {:valid? true :xml xml-path})))

(defn -main
  [& args]
  (cond
    (some #{"-h" "--help"} args)
    (println
     (str/join
      \newline
      ["Usage:"
       "  lein run -- [xsd-path] [out.edn] [--slice sewerage-mh|none] [--full] [--schema adac|landxml|bcib]"
       "  lein run -- xml-to-edn [schema.edn] [sample.xml] [out-instance.edn]"
       "  lein run -- edn-to-xml [schema.edn] [instance.edn] [out.xml]"
       "  lein run -- validate-xml [xsd-path] [document.xml]"
       ""
       "  Default ADAC XSD: resources/adac/ADAC_V600_Flattened.xsd"
       "  LandXML: lein run -- --schema landxml"
       "  BCIB:    lein run -- --schema bcib"
       "  Default sample XML: resources/adac/Sample-ADAC-V6.0.0.xml"]))

    (= "xml-to-edn" (first args))
    (let [result (xml-to-edn! {:schema (nth args 1 nil)
                               :xml (nth args 2 nil)
                               :out (nth args 3 nil)})]
      (println "Wrote" (:out result)))

    (= "edn-to-xml" (first args))
    (let [result (edn-to-xml! {:schema (nth args 1 nil)
                               :instance (nth args 2 nil)
                               :out (nth args 3 nil)})]
      (println "Wrote" (:out result)))

    (= "validate-xml" (first args))
    (let [result (validate-xml! {:xsd (nth args 1 nil)
                                 :xml (nth args 2 nil)})]
      (println "Valid" (:xml result)))

    :else
    (let [opts (parse-args args)
          result (convert! opts)]
      (println "Wrote" (:out result)
               "typedefs=" (:typedef-count result)
               "elements=" (:element-count result)
               "profile=" (pr-str (:profile result))
               "slice=" (pr-str (:slice result)))))
  (shutdown-agents))
