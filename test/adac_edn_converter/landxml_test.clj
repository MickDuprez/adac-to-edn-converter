(ns adac-edn-converter.landxml-test
  "LandXML-1.2 profile: ref resolution, simpleContent, convert smoke."
  (:require [adac-edn-converter.convert :as convert]
            [adac-edn-converter.map.elements :as el]
            [adac-edn-converter.xsd.parse :as parse]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]))

(def landxml-xsd-path
  (.getPath (io/resource "landxml/LandXML-1.2.xsd")))

(defn- xml-namespaced-keys
  [m]
  (filter #(and (keyword? %) (= "xml" (namespace %))) (keys m)))

(defn- by-id-map
  [bundle]
  (into {} (map (juxt :record/id identity) (:elements bundle))))

(defn- item-ref-of
  [coll]
  (or (get-in coll [:element/data :collection/item-ref])
      (first (:element/child-refs coll))))

(deftest parse-resolves-element-refs
  (let [schema (parse/parse-schema landxml-xsd-path)
        landxml (get-in schema [:elements "LandXML"])
        choice (first (get-in landxml [:inline-complex :particles]))
        units-ref (first (filter #(= "Units" (:ref %)) (:particles choice)))
        resolved (el/resolve-element-decl schema units-ref)]
    (is (contains? (:elements schema) "LandXML"))
    (is (= :choice (:kind choice)))
    (is (= :many (:max-occurs choice)))
    (is (= "Units" (:ref units-ref)))
    (is (= "Units" (:name resolved)))
    (is (map? (:inline-complex resolved)))
    (is (>= (count (filter :ref (:particles choice))) 10))))

(deftest simple-content-merges-point-type-attrs
  (let [schema (parse/parse-schema landxml-xsd-path)
        cg (get-in schema [:elements "CgPoint" :inline-complex])
        resolved (el/resolve-simple-content schema cg)
        attr-names (set (map :name (:attributes resolved)))]
    (is (= :simple-content (:kind cg)))
    (is (= "PointType" (:base cg)))
    (is (contains? attr-names "name"))
    (is (contains? attr-names "code"))
    (is (contains? attr-names "state"))
    (is (string? (:base resolved)))))

(deftest list-point-types-parsed
  (let [schema (parse/parse-schema landxml-xsd-path)
        st (:simple-types schema)]
    (is (= "xs:double" (get-in st ["Point" :list-item-type])))
    (is (= "Point" (get-in st ["Point3dOpt" :base])))
    (is (= "Point" (get-in st ["Point3dReq" :base])))))

(deftest convert-landxml-smoke
  (let [bundle (convert/convert-file landxml-xsd-path :profile :landxml :slice nil)
        schema (:schema bundle)
        by-name (group-by :record/name (:elements bundle))
        by-id (by-id-map bundle)
        root (first (by-name :LandXML))
        cg (first (filter #(= :sequence (:element/kind %)) (by-name :CgPoint)))
        cg-children (map by-id (:element/child-refs cg))
        cg-names (set (map :record/name cg-children))
        list-typedefs (filter #(get-in % [:typedef/facets :list?]) (:typedefs bundle))
        feature (first (filter #(= :sequence (:element/kind %)) (by-name :Feature)))
        feature-children (map by-id (:element/child-refs feature))
        feature-list (first (filter #(= :Feature_list (:record/name %)) feature-children))
        alignment (first (filter #(= :sequence (:element/kind %)) (by-name :Alignment)))
        alignment-list (first (by-name :Alignment_list))
        align-children (map by-id (:element/child-refs alignment))
        align-names (set (map :record/name align-children))
        coord (first (filter #(= :sequence (:element/kind %)) (by-name :CoordGeom)))
        coord-list (when coord
                     (first (filter #(= :CoordGeom_list (:record/name %))
                                    (map by-id (:element/child-refs coord)))))
        frag (when coord-list (by-id (item-ref-of coord-list)))
        frag-names (set (map #(:record/name (by-id %)) (or (:element/child-refs frag) [])))
        start (first (filter #(= :sequence (:element/kind %)) (by-name :Start)))
        start-names (set (map #(:record/name (by-id %)) (:element/child-refs start)))
        parcel (first (filter #(= :sequence (:element/kind %)) (by-name :Parcel)))
        parcel-names (set (map #(:record/name (by-id %)) (:element/child-refs parcel)))
        line (first (filter #(= :host-event (:element/kind %)) (by-name :Line)))
        curve (first (filter #(= :host-event (:element/kind %)) (by-name :Curve)))
        spiral (first (filter #(= :host-event (:element/kind %)) (by-name :Spiral)))
        irreg (first (filter #(= :host-event (:element/kind %)) (by-name :IrregularLine)))]
    (testing "schema metadata"
      (is (= "LandXML" (:record/name schema)))
      (is (= 1.2M (:schema/version schema)))
      (is (= convert/landxml-schema-id (:record/id schema))))
    (testing "root collection"
      (is (some? root))
      (is (= :collection (:element/kind root)))
      (is (= :many (get-in root [:element/cardinality :max])))
      (let [root-children (map by-id (:element/child-refs root))
            root-names (set (map :record/name root-children))]
        (is (contains? root-names :LandXML_Fragment))
        (is (contains? root-names :date))
        (is (contains? root-names :version))))
    (testing "structure counts and kinds"
      (is (>= (count (:typedefs bundle)) 50))
      (is (>= (count (:elements bundle)) 200))
      (is (every? #(contains? #{:scalar :sequence :collection :choice :host-event}
                              (:element/kind %))
                  (:elements bundle)))
      (is (empty? (filter #(= :host-event (:element/kind %))
                          (filter #(= :Geometry (:record/name %)) (:elements bundle)))))
      (is (every? empty? (map xml-namespaced-keys
                              (concat [schema]
                                      (:typedefs bundle)
                                      (:elements bundle))))))
    (testing "no element self-links in child-refs"
      (doseq [e (:elements bundle)]
        (is (not (some #{(:record/id e)} (:element/child-refs e)))
            (str (:record/name e) " must not include itself in child-refs"))))
    (testing "CgPoint inherits PointType attributes"
      (is (some? cg))
      (is (= :sequence (:element/kind cg)))
      (is (contains? cg-names :CgPoint_content))
      (is (contains? cg-names :name))
      (is (contains? cg-names :code))
      (is (contains? cg-names :state)))
    (testing "xs:list typedef facets"
      (is (seq list-typedefs))
      (is (some #(= :Point (:record/name %)) list-typedefs))
      (is (every? #(true? (get-in % [:typedef/facets :list?])) list-typedefs)))
    (testing "recursive Feature nests via Feature_list item-ref"
      (is (some? feature))
      (is (= :sequence (:element/kind feature)))
      (is (some? feature-list))
      (is (= :collection (:element/kind feature-list)))
      (is (= (:record/id feature) (item-ref-of feature-list))))
    (testing "Alignment_list item is a sequence form with CoordGeom and Feature_list"
      (is (some? alignment-list))
      (is (= :collection (:element/kind alignment-list)))
      (is (= (:record/id alignment) (item-ref-of alignment-list)))
      (is (= :sequence (:element/kind alignment)))
      (is (not= :collection (:element/kind alignment)))
      (is (contains? align-names :CoordGeom))
      (is (contains? align-names :Feature_list))
      (is (contains? align-names :name))
      (is (contains? align-names :length)))
    (testing "Parcel has CoordGeom and Feature_list as siblings"
      (is (some? parcel))
      (is (contains? parcel-names :CoordGeom))
      (is (contains? parcel-names :Feature_list)))
    (testing "CoordGeom is ordinary sequence around GeomList"
      (is (some? coord))
      (is (= :sequence (:element/kind coord)))
      (is (nil? (:element/host-action coord)))
      (is (some? coord-list))
      (is (= :collection (:element/kind coord-list)))
      (is (= :CoordGeom_list (:record/name coord-list)))
      (is (= :choice (:element/kind frag)))
      (is (= :CoordGeom_Fragment (:record/name frag)))
      (is (contains? frag-names :Line))
      (is (contains? frag-names :Curve))
      (is (contains? frag-names :Spiral))
      (is (contains? frag-names :IrregularLine))
      (is (contains? frag-names :Chain)))
    (testing "GeomList segments are :self host-events"
      (doseq [[el handler] [[line "onCaptureLine"]
                            [curve "onCaptureCurve"]
                            [spiral "onCaptureSpiral"]
                            [irreg "onCaptureIrregularLine"]]]
        (is (some? el))
        (is (= :host-event (:element/kind el)))
        (is (= :self (get-in el [:element/host-action :mode])))
        (is (= handler (get-in el [:element/host-action :handler])))
        (is (nil? (get-in el [:element/host-action :target-id])))
        (is (>= (count (:element/child-refs el)) 1)))
      (doseq [cid (:element/child-refs frag)]
        (let [child (by-id cid)]
          (when (contains? #{:Line :Curve :Spiral :IrregularLine} (:record/name child))
            (is (= :host-event (:element/kind child))
                (str (:record/name child) " should be a :self host-event")))
          (when (= :Chain (:record/name child))
            (is (contains? #{:sequence :choice :collection} (:element/kind child))
                "Chain remains an ordinary complex"))))
      (let [curve-el (by-id (first (filter #(= :Curve (:record/name (by-id %)))
                                           (:element/child-refs frag))))]
        (is (some? curve-el))
        (is (not= :Curve_list (:record/name curve-el)))))
    (testing "type lists do not point at collections"
      (doseq [n [:Alignment_list]
              :let [coll (first (by-name n))
                    item (by-id (item-ref-of coll))]]
        (is (some? item))
        (is (not= :collection (:element/kind item))
            (str n " item must not be a collection"))))
    (testing "soft pntRef on Start"
      (is (some? start))
      (is (contains? start-names :pntRef)))))

(deftest fixtures-exist-for-landxml-samples
  (is (.exists (io/file "test/fixtures/landxml-is185989.xml"))
      "cadastral sample copied from doc/landxml")
  (is (.exists (io/file "test/fixtures/landxml-coordgeom-curve.xml"))
      "synthetic Alignment/CoordGeom/Curve/Spiral fixture")
  (let [synth (slurp "test/fixtures/landxml-coordgeom-curve.xml")]
    (is (re-find #"CoordGeom" synth))
    (is (re-find #"<Curve " synth))
    (is (re-find #"<Spiral " synth))
    (is (re-find #"pntRef=" synth)))
  (let [sample (slurp "test/fixtures/landxml-is185989.xml")]
    (is (re-find #"CoordGeom" sample))
    (is (re-find #"pntRef=" sample))))
