(ns adac-edn-converter.edn.emit
  "Pretty-print SchemaCraft EDN bundles."
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]))

(defn write-edn
  "Write bundle to path with readable formatting."
  [bundle path]
  (with-open [w (io/writer path)]
    (binding [*out* w
              *print-namespace-maps* false
              *print-length* nil
              *print-level* nil]
      (pp/pprint bundle)))
  path)

(defn emit-str
  [bundle]
  (binding [*print-namespace-maps* false
            *print-length* nil
            *print-level* nil]
    (with-out-str (pp/pprint bundle))))
