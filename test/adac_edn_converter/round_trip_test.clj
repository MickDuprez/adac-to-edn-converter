(ns adac-edn-converter.round-trip-test
  (:require [adac-edn-converter.convert :as convert]
            [adac-edn-converter.instance.graph :as graph]
            [adac-edn-converter.instance.schema-index :as idx]
            [adac-edn-converter.instance.xml.read :as xml-read]
            [adac-edn-converter.instance.xml.write :as xml-write]
            [adac-edn-converter.xsd.parse :as parse]
            [adac-edn-converter.xsd.validate :as validate]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]))

(def xsd-path
  (.getPath (io/resource "adac/ADAC_V600_Flattened.xsd")))

(def sample-xml-path "resources/adac/Sample-ADAC-V6.0.0.xml")
(def fixture-xml-path "test/fixtures/sample-project-mh.xml")
(def invalid-fixture-xml-path "test/fixtures/sample-project-mh-invalid.xml")

(defn- bundle
  []
  (convert/convert (parse/parse-schema xsd-path) :slice nil))

(defn- round-trip-edn
  "XML path → EDN → XML string → EDN."
  [b xml-path]
  (let [instance (xml-read/read-file-path b xml-path)
        xml-out (xml-write/emit-str (idx/build-index b) instance)]
    (xml-read/read-file-path b
                             (java.io.ByteArrayInputStream.
                              (.getBytes xml-out "UTF-8")))))

(defn- sample-stable-subset
  "Stable paths for Sample ADAC structural equality (not full document)."
  [instance]
  (let [mh (get-in instance [:Project :ProjectData :Sewerage :MaintenanceHoles 0])]
    {:version (:version instance)
     :project-name (get-in instance [:Project :Name])
     :mh-id (:ADACId mh)
     :mh-use (:Use mh)
     :point-x (some-> mh (get-in [:Geometry :Point :X]) double)
     :point-y (some-> mh (get-in [:Geometry :Point :Y]) double)
     :surveyor-nil? (= :schemacraft/nil (get-in mh [:ComponentInfo :Surveyor]))}))

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

(deftest sample-xml-round-trip-stable-subset
  (let [b (bundle)
        before (xml-read/read-file-path b sample-xml-path)
        after (round-trip-edn b sample-xml-path)]
    (is (= (sample-stable-subset before) (sample-stable-subset after)))))

(deftest fixture-xml-round-trip-structure
  (let [b (bundle)
        instance (xml-read/read-file-path b fixture-xml-path)
        round (round-trip-edn b fixture-xml-path)
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
        (validate/validation-error xsd-path (str out)))
    (let [xml (slurp out)]
      (is (re-find #"xmlns=\"http://www.adac.com.au\"" xml))
      (is (re-find #"xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"" xml))
      (is (re-find #"xsi:nil=\"true\"" xml))
      (is (not (re-find #"xmlns:a=" xml))))))

(deftest invalid-fixture-imports-without-throw
  (let [b (bundle)
        instance (xml-read/read-file-path b invalid-fixture-xml-path)
        mh (get-in instance [:Project :ProjectData :Sewerage :MaintenanceHoles 0])]
    (testing "imports XSD-invalid but well-formed XML"
      (is (= "6.0.0" (:version instance)))
      (is (= "Invalid fixture project" (get-in instance [:Project :Name])))
      (is (= "bad/1" (:ADACId mh)))
      (is (= "NotAValidUseValue" (:Use mh)))
      (is (= "not-a-number" (:InvertLevel_m mh)))
      (is (= 100.5 (double (get-in mh [:Geometry :Point :X]))))
      (is (nil? (get mh :UnknownExtraField)) "unknown tags dropped")
      (is (nil? (:ChamberSize mh)) "missing required choice omitted, no throw"))
    (testing "export still emits well-formed XML; re-import keeps bad values"
      (let [xml-out (xml-write/emit-str (idx/build-index b) instance)
            round (xml-read/read-file-path
                   b
                   (java.io.ByteArrayInputStream. (.getBytes xml-out "UTF-8")))
            mh2 (get-in round [:Project :ProjectData :Sewerage :MaintenanceHoles 0])
            out (io/file "target/test-roundtrip-mh-invalid.xml")]
        (is (string? xml-out))
        (is (re-find #":ADAC|ADAC" xml-out))
        (is (= "NotAValidUseValue" (:Use mh2)))
        (is (= "not-a-number" (:InvertLevel_m mh2)))
        (xml-write/write-file b instance (str out))
        (is (false? (validate/valid? xsd-path (str out)))
            "exported invalid fixture must still fail XSD (SchemaCraft owns validity)")))))

(defn- strip-choice-selected
  "XML assemble omits :choice/selected; graph round-trip may re-add it."
  [v]
  (cond
    (map? v) (into {}
                   (keep (fn [[k x]]
                           (when (not= k :choice/selected)
                             [k (strip-choice-selected x)])))
                   v)
    (sequential? v) (mapv strip-choice-selected v)
    :else v))

(deftest sample-xml-to-instance-graph-round-trip
  (let [b (bundle)
        assembled (xml-read/read-file-path b sample-xml-path)
        g (graph/assembled->document-graph b assembled)
        back (graph/graph->assembled b g)
        root (first (filter :instance/root? (:schemacraft/instances g)))
        adac (first (filter #(= :ADAC (:instance/element %))
                            (:schemacraft/instances g)))]
    (is (= :instance-graph (:schemacraft/format g)))
    (is (= 1 (:schemacraft/version g)))
    (is (= :document (:schemacraft/kind g)))
    (is (= (:record/id (:schema b)) (get-in g [:schemacraft/schema :id])))
    (is (= "ADAC" (get-in g [:schemacraft/schema :name])))
    (is (= :Document (:instance/element root)))
    (is (true? (:instance/root? root)))
    (is (= (:instance/id root) (get-in g [:schemacraft/document :id])))
    (is (= "Example ADAC capture" (:instance/label root)))
    (is (= :ADAC (:instance/element adac)))
    (is (= (:instance/id root) (:instance/parent-id adac)))
    (is (= :ADAC (:instance/member-key adac)))
    (is (= "6.0.0" (get-in adac [:instance/value :version])))
    (is (= (sample-stable-subset assembled)
           (sample-stable-subset back)))
    (is (= (strip-choice-selected assembled)
           (strip-choice-selected back)))
    (testing "graph → XML → assembled keeps stable subset"
      (let [xml-out (xml-write/emit-str (idx/build-index b) back)
            again (xml-read/read-file-path
                   b
                   (java.io.ByteArrayInputStream. (.getBytes xml-out "UTF-8")))]
        (is (= (sample-stable-subset assembled)
               (sample-stable-subset again)))))))

(deftest fixture-instance-graph-choice-and-collection
  (let [b (bundle)
        assembled (xml-read/read-file-path b fixture-xml-path)
        g (graph/assembled->document-graph b assembled)
        back (graph/graph->assembled b g)
        mh (get-in back [:Project :ProjectData :Sewerage :MaintenanceHoles 0])
        choice-rows (filter #(= :ChamberSize (:instance/element %))
                            (:schemacraft/instances g))]
    (is (= 1 (count choice-rows)))
    (is (= :Circular (get-in (first choice-rows) [:instance/value :choice/selected])))
    (is (= :Circular (:choice/selected (:ChamberSize mh))))
    (is (= 1050 (get-in mh [:ChamberSize :Circular :Diameter_mm])))
    (is (integer? (:instance/member-index
                   (first (filter #(= :MaintenanceHole (:instance/element %))
                                  (:schemacraft/instances g))))))))

(defn- geometry-counts
  [assembled]
  (let [*pts (atom 0)
        *pl (atom 0)
        *pg (atom 0)
        *nil (atom 0)]
    (letfn [(walk [v]
              (cond
                (and (map? v) (contains? v :Geometry))
                (let [g (:Geometry v)]
                  (cond
                    (or (= g {:Polyline nil}) (= g {:Polygon nil})) (swap! *nil inc)
                    (get g :Point) (swap! *pts inc)
                    (get g :Polyline) (swap! *pl inc)
                    (get g :Polygon) (swap! *pg inc))
                  (doseq [[_ x] v] (walk x)))
                (map? v) (doseq [[_ x] v] (walk x))
                (sequential? v) (doseq [x v] (walk x))))]
      (walk assembled)
      {:point @*pts :polyline @*pl :polygon @*pg :nil-geom @*nil})))

(deftest schema-has-no-empty-complex-children
  (let [b (bundle)
        empty (->> (:elements b)
                   (filter #(and (= :complex (:element/type %))
                                 (#{:sequence :collection :choice} (:element/kind %))
                                 (empty? (:element/child-refs %))))
                   (mapv :record/name))]
    (is (= [] empty) "complex Path/Ring/group refs must expand"))
  (let [b (bundle)
        paths (filter #(and (= :Path (:record/name %))
                            (re-find #"complex curves" (str (:record/documentation %))))
                      (:elements b))]
    (is (seq paths))
    (is (every? #(seq (:element/child-refs %)) paths)))
  (let [b (bundle)
        ls (filter #(= :LandStabilisation (:record/name %)) (:elements b))
        coll (first (filter #(= :collection (:element/kind %)) ls))
        item-id (get-in coll [:element/data :collection/item-ref])]
    (is (some? coll))
    (is (not= (:record/id coll) item-id) "same-name collection must not self-ref")))

(deftest sample-full-semantic-round-trip
  (let [b (bundle)
        before (xml-read/read-file-path b sample-xml-path)
        xml-out (xml-write/emit-str (idx/build-index b) before)
        after (xml-read/read-file-path
               b (java.io.ByteArrayInputStream. (.getBytes xml-out "UTF-8")))
        g (graph/assembled->document-graph b before)
        graph-back (graph/graph->assembled b g)
        out (io/file "target/test-sample-roundtrip.xml")
        geoms (geometry-counts before)]
    (is (= (strip-choice-selected before) (strip-choice-selected after))
        "full Sample XML→EDN→XML→EDN semantic equality")
    (is (= (strip-choice-selected before) (strip-choice-selected graph-back))
        "full Sample assembled↔instance-graph equality")
    (is (zero? (:nil-geom geoms)) "no dropped Polyline/Polygon Geometry")
    (is (pos? (:point geoms)))
    (is (pos? (:polyline geoms)))
    (is (pos? (:polygon geoms)))
    (xml-write/write-file b before (str out))
    (is (true? (validate/valid? xsd-path (str out)))
        (str "Sample export must validate: "
             (validate/validation-error xsd-path (str out))))))
