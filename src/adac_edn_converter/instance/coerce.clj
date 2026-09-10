(ns adac-edn-converter.instance.coerce
  "Coerce XML text ↔ SchemaCraft Instance scalars using TypeDef metadata.

  Lenient: parse failures never throw — return the raw trimmed string so
  XSD-invalid values (bad enums, non-numeric text, …) survive import for
  SchemaCraft to highlight. Export formats whatever is stored."
  (:require [clojure.string :as str])
  (:import [java.math BigDecimal]))

(defn- trim-str [s]
  (when s (str/trim (str s))))

(defn coerce-value
  "Coerce string + typedef record → Instance value.
  On failure (or unknown primitive), returns the trimmed raw string (or nil)."
  [s typedef]
  (try
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
        :decimal (try (BigDecimal. ^String raw) (catch Exception _ raw))
        :float (try (Double/parseDouble raw) (catch Exception _ raw))
        :date raw
        :time raw
        :date-time raw
        :enum raw
        raw))
    (catch Exception _
      (trim-str s))))

(defn- typedef-primitive
  [typedef]
  (or (:typedef/primitive typedef)
      (get-in typedef [:element/data :type/primitive])
      :string))

(defn- coerce-boolean-token
  [x]
  (case (str/lower-case (str x))
    ("true" "1") true
    ("false" "0") false
    nil))

(defn format-value
  "Format Instance value → XML text for export. Never throws."
  [v typedef]
  (try
    (cond
      (nil? v) nil
      (and (sequential? v) (= :boolean (typedef-primitive typedef)))
      (format-value (or (first (keep coerce-boolean-token v)) false) typedef)
      (instance? BigDecimal v) (.toPlainString ^BigDecimal v)
      (number? v) (str v)
      (boolean? v) (if v "true" "false")
      :else (str v))
    (catch Exception _
      (str v))))
