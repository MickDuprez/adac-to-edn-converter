(ns adac-edn-converter.round-trip-test
  (:require [adac-edn-converter.convert :as convert]
            [adac-edn-converter.instance.schema-index :as idx]
            [adac-edn-converter.instance.xml.read :as xml-read]
            [adac-edn-converter.instance.xml.write :as xml-write]
            [adac-edn-converter.xsd.parse :as parse]
            [adac-edn-converter.xsd.validate :as validate]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]))

(def xsd-path
  (.getPath (io/resource "adac/ADAC_V600_Flattened.xsd")))

(def sample-xml-path "resources/adac/Sample-ADAC-V6.0.0.xml")
(def fixture-xml-path "test/fixtures/sample-project-mh.xml")

(defn- bundle
  []
  (convert/convert (parse/parse-schema xsd-path) :slice nil))

(deftest full-adac-schema-conversion
  (let [b (bundle)
        names (set (map :record/name (:elements b)))
        version (some #(when (= :version (:record/name %)) %) (:elements b))
        geometries (filter #(= :Geometry (:record/name %)) (:elements b))
        host-geometries (filter #(= :host-event (:element/kind %)) geometries)]
    (is (>= (count (:typedefs b)) 200))
    (is (>= (count (:elements b)) 900))
    (is (contains? names :Project))
    (is (contains? names :MaintenanceHole))
    (is (seq host-geometries))
    (is (= "6.0.0" (:element/fixed version)))
    (is (every? #(contains? #{:scalar :sequence :collection :choice :host-event}
                            (:element/kind %))
                (:elements b)))))

(deftest schema-index-resolves-root-and-children
  (let [index (idx/build-index (bundle))
        root (idx/root-element index)
        children (idx/children index root)]
    (is (= :ADAC (:record/name root)))
    (is (some #(= :Project (:record/name %)) children))
    (is (some #(= :version (:record/name %)) children))))

(deftest sample-xml-imports-project-and-geometry
  (let [b (bundle)
        instance (xml-read/read-file-path b sample-xml-path)
        mh (get-in instance [:Project :ProjectData :Sewerage :MaintenanceHoles 0])]
    (is (= "6.0.0" (:version instance)))
    (is (= "Example ADAC capture" (get-in instance [:Project :Name])))
    (is (= :schemacraft/nil (get-in mh [:ComponentInfo :Surveyor])))
    (is (= 495478.066 (double (get-in mh [:Geometry :Point :X]))))))

(deftest fixture-xml-round-trip-structure
  (let [b (bundle)
        instance (xml-read/read-file-path b fixture-xml-path)
        round (xml-read/read-file-path b
                                       (java.io.ByteArrayInputStream.
                                        (.getBytes (xml-write/emit-str (idx/build-index b) instance)
                                                   "UTF-8")))
        mh (get-in round [:Project :ProjectData :Sewerage :MaintenanceHoles 0])]
    (is (= "6.0.0" (:version round)))
    (is (= "3/1" (:ADACId mh)))
    (is (= :Circular (ffirst (:ChamberSize mh))))
    (is (= 1050 (get-in mh [:ChamberSize :Circular :Diameter_mm])))
    (is (= 495478.066 (double (get-in mh [:Geometry :Point :X]))))))

(deftest fixture-round-trip-validates-against-xsd
  (let [b (bundle)
        instance (xml-read/read-file-path b fixture-xml-path)
        out (io/file "target/test-roundtrip-mh.xml")]
    (xml-write/write-file b instance (str out))
    (is (true? (validate/valid? xsd-path (str out)))
        (validate/validation-error xsd-path (str out)))))
