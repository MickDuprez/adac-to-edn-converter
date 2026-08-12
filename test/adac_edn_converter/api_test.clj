(ns adac-edn-converter.api-test
  (:require [adac-edn-converter.api :as api]
            [adac-edn-converter.convert :as convert]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]))

(def ^:private sample-v6 "resources/adac/Sample-ADAC-V6.0.0.xml")
(def ^:private sample-v5 "resources/adac/ADAC_v501_XSD/Sample ADAC V5.0.1.xml")

(deftest packaged-schema-ids-distinct
  (let [packs (api/packaged-schema-edns)
        ids (mapv :id packs)]
    (is (= 2 (count packs)))
    (is (= 2 (count (set ids))))
    (is (= convert/adac-schema-id-v600 (first ids)))
    (is (= convert/adac-schema-id-v501 (second ids)))))

(deftest peek-version
  (is (= "6.0.0" (api/peek-adac-version sample-v6)))
  (is (= "5.0.1" (api/peek-adac-version sample-v5)))
  (is (= convert/adac-schema-id-v600 (api/suggest-schema-id sample-v6)))
  (is (= convert/adac-schema-id-v501 (api/suggest-schema-id sample-v5))))

(deftest convert-v6-native
  (let [r (api/convert-adac-xml sample-v6
                                {:target-schema-id convert/adac-schema-id-v600
                                 :filename "Sample-ADAC-V6.0.0.xml"})]
    (is (:valid? r))
    (is (not (:upgraded? r)))
    (is (= :instance-graph (get-in r [:envelope :schemacraft/format])))
    (is (= convert/adac-schema-id-v600
           (get-in r [:envelope :schemacraft/schema :id])))
    (is (= "6.0.0" (get-in r [:assembled :version])))))

(deftest convert-v5-native
  (let [r (api/convert-adac-xml sample-v5
                                {:target-schema-id convert/adac-schema-id-v501
                                 :filename "Sample ADAC V5.0.1.xml"})]
    (is (:valid? r))
    (is (not (:upgraded? r)))
    (is (= convert/adac-schema-id-v501
           (get-in r [:envelope :schemacraft/schema :id])))
    (is (= "5.0.1" (get-in r [:assembled :version])))))

(deftest convert-v5-upgrade-to-v6
  (let [r (api/convert-adac-xml sample-v5
                                {:target-schema-id convert/adac-schema-id-v600
                                 :filename "Sample ADAC V5.0.1.xml"})]
    (is (:valid? r))
    (is (:upgraded? r))
    (is (= convert/adac-schema-id-v600
           (get-in r [:envelope :schemacraft/schema :id])))
    (is (= "6.0.0" (get-in r [:assembled :version])))
    (is (map? (:envelope r)))
    (is (seq (get-in r [:envelope :schemacraft/instances])))))

(deftest convert-v6-into-v501-rejected
  (let [r (api/convert-adac-xml sample-v6
                                {:target-schema-id convert/adac-schema-id-v501})]
    (is (not (:valid? r)))
    (is (string? (:error r)))))

(deftest export-adac-xml-sample-valid
  (let [assembled (api/xml->assembled (api/load-schema-bundle :v600) sample-v6)
        r (api/export-adac-xml :v600 assembled)]
    (is (true? (:valid? r)))
    (is (empty? (:errors r)))
    (is (string? (:xml r)))
    (is (re-find #"<a?:?ADAC" (:xml r)))
    (is (string? (:xsd-path r)))))

(deftest export-adac-xml-document-wrapper
  (let [assembled (api/xml->assembled (api/load-schema-bundle :v600) sample-v6)
        r (api/export-adac-xml convert/adac-schema-id-v600 {:ADAC assembled})]
    (is (true? (:valid? r)))
    (is (empty? (:errors r)))))

(deftest export-adac-xml-xsd-errors-soft
  (let [bad {:version "6.0.0"
             :Project {:Name "Bad"
                       :Owner "X"
                       :ProjectData
                       {:Sewerage
                        {:MaintenanceHoles
                         [{:ADACId "x/1"
                           :Use "NotAValidUseValue"
                           :InvertLevel_m "not-a-number"}]}}}}
        r (api/export-adac-xml :v600 bad)]
    (is (false? (:valid? r)))
    (is (seq (:errors r)))
    (is (string? (:xml r)))
    (is (re-find #"ADAC" (:xml r)))))
