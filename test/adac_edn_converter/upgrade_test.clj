(ns adac-edn-converter.upgrade-test
  (:require [adac-edn-converter.api :as api]
            [adac-edn-converter.instance.xml.read :as xml-read]
            [adac-edn-converter.upgrade :as up]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]))

(def ^:private sample-v5
  "resources/adac/ADAC_v501_XSD/Sample ADAC V5.0.1.xml")

(defn- upgraded-sample []
  (let [b5 (api/load-schema-bundle :v501)
        b6 (api/load-schema-bundle :v600)
        a5 (xml-read/read-file-path b5 sample-v5)]
    (up/upgrade-v501->v600 b6 a5)))

(deftest upgrade-keeps-polygon-vertices
  (let [a6 (upgraded-sample)
        geom (get-in a6 [:Project :ProjectData :Cadastre :LandParcels 0 :Lot :Geometry])
        ring0 (get-in geom [:Polygon 0 0])
        verts (:PolySegment ring0)]
    (is (map? geom))
    (is (= :PolySegment (:choice/selected ring0)))
    (is (vector? verts))
    (is (>= (count verts) 3))
    (is (number? (:X (first verts))))
    (is (number? (:Y (first verts))))))

(deftest upgrade-keeps-circpipe-and-circular-diameter
  (let [a6 (upgraded-sample)
        pipe (get-in a6 [:Project :ProjectData :StormWater :Pipes 0 :PipeStructure])
        mh (get-in a6 [:Project :ProjectData :Sewerage :MaintenanceHoles 0 :ChamberSize])]
    (is (= 225 (get-in pipe [:CircPipe :Diameter_mm])))
    (is (= :CircPipe (:choice/selected pipe)))
    (is (= 1050 (get-in mh [:Circular :Diameter_mm])))
    (is (= :Circular (:choice/selected mh)))))

(deftest upgrade-electrical-conduit-uses-size-mm
  (let [a6 (upgraded-sample)
        conduits (get-in a6 [:Project :ProjectData :Electrical :Conduits])
        c0 (first conduits)]
    (testing "OpenSpace electrical relocated under Electrical with Size_mm"
      (is (seq conduits))
      (is (contains? c0 :Size_mm))
      (is (not (contains? c0 :Diameter_mm)))
      (is (number? (:Size_mm c0))))))

(deftest convert-adac-xml-upgrade-path-preserves-geom-and-diameter
  (let [r (api/convert-adac-xml sample-v5
                                {:target-schema-id api/schema-id-v600
                                 :filename "Sample ADAC V5.0.1.xml"})
        a (:assembled r)]
    (is (:valid? r))
    (is (:upgraded? r))
    (is (seq (get-in a [:Project :ProjectData :Cadastre :LandParcels 0
                        :Lot :Geometry :Polygon 0 0 :PolySegment])))
    (is (= 225 (get-in a [:Project :ProjectData :StormWater :Pipes 0
                          :PipeStructure :CircPipe :Diameter_mm])))))
