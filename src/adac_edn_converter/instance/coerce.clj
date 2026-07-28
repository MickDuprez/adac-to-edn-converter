(ns adac-edn-converter.instance.coerce
  "Coerce XML text to SchemaCraft Instance values using TypeDef metadata."
  (:require [clojure.string :as str])
  (:import [java.math BigDecimal]))

(defn- trim-str [s]
  (when s (str/trim (str s))))

(defn coerce-value
  "Coerce string + typedef record → Instance value."
  [s typedef]
  (let [prim (or (:typedef/primitive typedef)
                 (get-in typedef [:element/data :type/primitive])
                 :string)
        raw (trim-str s)]
    (case prim
      :string raw
      :boolean (case (str/lower-case (or raw ""))
                 "true" true
                 "false" false
                 "1" true
                 "0" false
                 raw)
      :integer (try (Long/parseLong raw) (catch Exception _ raw))
      :decimal (try (BigDecimal. raw) (catch Exception _ raw))
      :float (try (Double/parseDouble raw) (catch Exception _ raw))
      :date raw
      :time raw
      :date-time raw
      :enum raw
      raw)))

(defn format-value
  "Format Instance value → XML text for export."
  [v typedef]
  (cond
    (nil? v) nil
    (instance? BigDecimal v) (.toPlainString ^BigDecimal v)
    (number? v) (str v)
    (boolean? v) (if v "true" "false")
    :else (str v)))
