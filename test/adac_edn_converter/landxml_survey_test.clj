(ns adac-edn-converter.landxml-survey-test
  "LandXML instance XML → ADAC Geometry survey overlay."
  (:require [adac-edn-converter.api :as api]
            [adac-edn-converter.core :as core]
            [adac-edn-converter.landxml.coords :as coords]
            [adac-edn-converter.landxml.survey :as survey]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]))

(def curve-xml "test/fixtures/landxml-coordgeom-curve.xml")
(def cadastral-xml "test/fixtures/landxml-is185989.xml")
(def planfeature-xml "test/fixtures/landxml-planfeature-chain.xml")

(defn- by-id
  [catalog id]
  (some #(when (= id (:id %)) %) (:objects catalog)))

(defn- by-source
  [catalog source]
  (filterv #(= source (:source %)) (:objects catalog)))

(deftest ne-text-maps-to-adac-xy
  (is (= {:X 2000.0 :Y 1000.0 :Z 10.0}
         (coords/parse-point-text "1000 2000 10")))
  (is (= {:X 2000.0 :Y 1000.0}
         (coords/parse-point-text "1000 2000"))))

(deftest cgpoint-ne-to-xy
  (let [cat (survey/read-file-path curve-xml)
        p1 (by-id cat "P1")]
    (is (= :landxml-survey-geometry (:schemacraft/format cat)))
    (is (= "1.2" (:landxml/version cat)))
    (is (= :point (:kind p1)))
    (is (= :cg-point (:source p1)))
    (is (= {:X 2000.0 :Y 1000.0 :Z 10.0}
           (get-in p1 [:Geometry :Point])))))

(deftest alignment-line-curve-spiral
  (let [cat (survey/read-file-path curve-xml)
        cl (by-id cat "CL")
        path (get-in cl [:Geometry :Polyline :Path])
        warnings (:warnings cat)]
    (is (= :polyline (:kind cl)))
    (is (= :alignment (:source cl)))
    (is (vector? path))
    (is (= {:PolySegment [{:X 2000.0 :Y 1000.0 :Z 10.0}
                          {:X 2000.0 :Y 1100.0 :Z 10.0}]}
           (first path)))
    (is (= {:FromPoint {:X 2000.0 :Y 1100.0 :Z 10.0}
            :ToPoint {:X 2050.0 :Y 1200.0 :Z 10.0}
            :CentrePoint {:X 2500.0 :Y 1150.0 :Z 10.0}
            :Clockwise true}
           (:CurveCircular (second path))))
    (is (= 2 (count path)))
    (is (some #(re-find #"Spiral" %) warnings))
    (is (= ["P1" "P2" "P2" "P3"] (:pnt-refs cl)))))

(deftest planfeature-chain-and-location
  (let [cat (survey/read-file-path planfeature-xml)
        kerb (by-id cat "KERB-01")
        pipe (by-id cat "PIPE-01")
        lot (by-id cat "LOT-01")
        tree (by-id cat "TREE-01")]
    (is (= "GDA2020" (get-in cat [:coordinate-system :datum])))
    (is (= "MGA2020-56" (get-in cat [:coordinate-system :horizontal-datum])))
    (testing "open PlanFeature lines merge to one PolySegment"
      (is (= :polyline (:kind kerb)))
      (is (= :plan-feature (:source kerb)))
      (is (= "KB" (:code kerb)))
      (is (= [{:X 2000.0 :Y 1000.0 :Z 10.0}
              {:X 2000.0 :Y 1100.0 :Z 10.0}
              {:X 2100.0 :Y 1200.0 :Z 10.0}]
             (get-in kerb [:Geometry :Polyline :Path :PolySegment]))))
    (testing "Chain of CgPoint names"
      (is (= :polyline (:kind pipe)))
      (is (= "SEW" (:code pipe)))
      (is (= ["P1" "P2" "P3"] (:pnt-refs pipe)))
      (is (= (get-in kerb [:Geometry :Polyline :Path :PolySegment])
             (get-in pipe [:Geometry :Polyline :Path :PolySegment]))))
    (testing "closed PlanFeature becomes a Polygon ring"
      (is (= :polygon (:kind lot)))
      (is (= 4 (count (get-in lot [:Geometry :Polygon :Ring :PolySegment])))))
    (testing "Location-only PlanFeature is a point"
      (is (= :point (:kind tree)))
      (is (= {:X 2100.0 :Y 1000.0 :Z 10.0}
             (get-in tree [:Geometry :Point]))))
    (is (= 4 (count (by-source cat :cg-point))))
    (is (= 4 (count (by-source cat :plan-feature))))))

(deftest cadastral-parcel-polygon
  (let [cat (survey/read-file-path cadastral-xml)
        lot (by-id cat "25/RP726990")
        admin (by-id cat "7330WHITSUNDAY SHIRE")
        ring (get-in lot [:Geometry :Polygon :Ring :PolySegment])]
    (is (= "0" (:landxml/version cat)))
    (is (nil? admin) "administrative parcels without CoordGeom are skipped")
    (is (= :polygon (:kind lot)))
    (is (= :parcel (:source lot)))
    (is (string? (:class lot)))
    (is (string? (:name lot)))
    (is (= "Y" (get-in lot [:properties "areaSurveyedFlag"])))
    (is (= "Freehold" (get-in lot [:properties "dcdbTenureRecord"])))
    (is (= 5 (count ring)))
    (is (every? #(and (number? (:X %)) (number? (:Y %))) ring))
    (is (pos? (count (by-source cat :parcel))))
    (is (pos? (count (by-source cat :cg-point))))))

(deftest unresolved-pntref-warns
  (let [xml (str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                 "<LandXML version=\"1.2\">"
                 "<CgPoints>"
                 "<CgPoint name=\"P1\">1000 2000 10</CgPoint>"
                 "</CgPoints>"
                 "<PlanFeatures>"
                 "<PlanFeature name=\"BROKEN\">"
                 "<CoordGeom>"
                 "<Line><Start pntRef=\"P1\"/><End pntRef=\"MISSING\"/></Line>"
                 "</CoordGeom>"
                 "</PlanFeature>"
                 "</PlanFeatures>"
                 "</LandXML>")
        cat (api/landxml->survey-geometry xml)
        broken (by-id cat "BROKEN")]
    (is (some #(re-find #"Unresolved pntRef \"MISSING\"" %) (:warnings cat)))
    (is (some? (by-id cat "P1")))
    (is (some? broken) "named linework is returned even when vertices are missing")
    (is (nil? (:Geometry broken)))
    (is (some #(= "MISSING" %) (:pnt-refs broken)))))

(deftest api-accepts-file-path
  (let [cat (api/landxml->survey-geometry curve-xml)]
    (is (= :landxml-survey-geometry (:schemacraft/format cat)))
    (is (some? (by-id cat "P1")))
    (is (some? (by-id cat "CL")))))

(deftest cli-writes-catalog-edn
  (let [out "target/landxml-survey-test.edn"
        result (core/landxml-to-geometry! {:xml planfeature-xml :out out})]
    (is (= out (:out result)))
    (is (= 8 (:object-count result)))
    (is (.exists (io/file out)))))
