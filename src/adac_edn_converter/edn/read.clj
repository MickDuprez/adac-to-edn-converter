(ns adac-edn-converter.edn.read
  "Read SchemaCraft EDN bundles and instance files."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

(def edn-readers
  {'uuid #(java.util.UUID/fromString %)})

(defn read-edn
  [source]
  (edn/read-string {:readers edn-readers} (slurp source)))

(defn read-file
  [path]
  (read-edn (io/file path)))
