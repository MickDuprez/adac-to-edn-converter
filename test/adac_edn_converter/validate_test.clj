(ns adac-edn-converter.validate-test
  (:require [adac-edn-converter.api :as api]
            [adac-edn-converter.xsd.validate :as validate]
            [clojure.test :refer [deftest is testing]]))

(def ^:private sample-v6 "resources/adac/Sample-ADAC-V6.0.0.xml")
(def ^:private invalid-fixture "test/fixtures/sample-project-mh-invalid.xml")

(deftest validate-report-multi-error
  (let [xsd (api/resolve-xsd-path :v600)
        report (validate/validate-report xsd invalid-fixture)]
    (is (false? (:valid? report)))
    (is (>= (count (:errors report)) 2)
        "collects multiple XSD issues without throw-on-first")
    (is (every? #(contains? #{:warning :error :fatal} (:severity %))
                (:errors report)))
    (is (every? #(contains? % :message) (:errors report)))))

(deftest validate-report-sample-clean
  (let [xsd (api/resolve-xsd-path :v600)
        exported (api/export-adac-xml :v600
                                      (api/xml->assembled
                                       (api/load-schema-bundle :v600)
                                       sample-v6))
        report (validate/validate-report xsd (:xml exported))]
    (is (true? (:valid? exported)))
    (is (true? (:valid? report)))
    (is (empty? (filter #(#{:error :fatal} (:severity %)) (:errors report))))))
