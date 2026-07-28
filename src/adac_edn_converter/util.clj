(ns adac-edn-converter.util
  "Shared helpers: deterministic UUIDs, naming, documentation."
  (:require [clojure.string :as str])
  (:import (java.nio.charset StandardCharsets)
           (java.util UUID)))

(def ^:private uuid-ns
  "Namespace UUID for deterministic SchemaCraft record ids."
  (UUID/fromString "a6a6a6a6-0000-4000-8000-000000000001"))

(defn- uuid->bytes
  [^UUID u]
  (let [bb (java.nio.ByteBuffer/allocate 16)]
    (.putLong bb (.getMostSignificantBits u))
    (.putLong bb (.getLeastSignificantBits u))
    (.array bb)))

(defn stable-uuid
  "Deterministic UUID v5 from a namespaced string key."
  [s]
  (let [md (doto (java.security.MessageDigest/getInstance "SHA-1")
             (.update (uuid->bytes uuid-ns))
             (.update (.getBytes (str s) StandardCharsets/UTF_8)))
        dig (.digest md)]
    (aset dig 6 (unchecked-byte (bit-or (bit-and (aget dig 6) 0x0f) 0x50)))
    (aset dig 8 (unchecked-byte (bit-or (bit-and (aget dig 8) 0x3f) 0x80)))
    (let [bb (java.nio.ByteBuffer/wrap dig 0 16)]
      (UUID. (.getLong bb) (.getLong bb)))))

(defn local-name
  "Keyword or string → string local name (strip xmlns if present)."
  [x]
  (cond
    (nil? x) nil
    (keyword? x) (name x)
    (string? x) (if-let [i (str/last-index-of x ":")]
                  (subs x (inc i))
                  x)
    :else (str x)))

(defn as-keyword
  "XSD local name → Clojure keyword (preserves underscores)."
  [name]
  (when name
    (keyword (local-name name))))

(defn- title-token
  "Title-case a name token; leave already-all-caps tokens (e.g. CI) unchanged."
  [tok]
  (if (and (not (str/blank? tok))
           (= tok (str/upper-case tok))
           (re-find #"[A-Za-z]" tok))
    tok
    (str/capitalize tok)))

(defn element-label
  "UI label from XSD name: split on _, title-case tokens. Keys/names stay exact.
  Names without underscores are left unchanged (no CamelCase split)."
  [name]
  (let [local (local-name name)]
    (when local
      (if (str/includes? local "_")
        (->> (str/split local #"_")
             (remove str/blank?)
             (map title-token)
             (str/join " "))
        local))))

(defn parse-occurs
  [s]
  (cond
    (nil? s) 1
    (= s "unbounded") :many
    :else (Long/parseLong (str s))))

(defn xs-qname?
  [type-name]
  (when type-name
    (let [n (local-name type-name)
          s (str type-name)]
      (or (str/starts-with? s "xs:")
          (str/starts-with? s "xsd:")
          (contains? #{"string" "boolean" "decimal" "float" "double"
                       "integer" "int" "long" "short" "byte"
                       "nonNegativeInteger" "positiveInteger" "negativeInteger"
                       "nonPositiveInteger"
                       "date" "time" "dateTime" "duration" "gYear" "gMonth" "gDay"
                       "anyURI" "base64Binary" "hexBinary" "QName" "NOTATION"
                       "token" "normalizedString" "language" "Name" "NCName"
                       "NMTOKEN" "NMTOKENS" "ID" "IDREF" "IDREFS" "ENTITY" "ENTITIES"}
                     n)))))

(defn builtin-primitive
  "Map XS builtin type local name → SchemaCraft :typedef/primitive."
  [type-name]
  (case (local-name type-name)
    ("string" "normalizedString" "token" "language" "Name" "NCName"
     "NMTOKEN" "anyURI" "ID" "IDREF" "ENTITY") :string
    ("boolean") :boolean
    ("decimal") :decimal
    ("float" "double") :float
    ("integer" "int" "long" "short" "byte"
     "nonNegativeInteger" "positiveInteger" "negativeInteger"
     "nonPositiveInteger") :integer
    ("date") :date
    ("time") :time
    ("dateTime") :date-time
    :string))
