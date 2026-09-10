(ns adac-edn-converter.instance.xml-util
  "Shared XML helpers for ADAC instance I/O."
  (:require [clojure.data.xml :as xml]
            [clojure.string :as str]))

(def adac-ns "http://www.adac.com.au")
(def xsi-ns "http://www.w3.org/2001/XMLSchema-instance")

(defn encode-xmlns-uri
  [uri]
  (-> uri
      (java.net.URLEncoder/encode "UTF-8")
      (str/replace "+" "%20")))

(def xmlns-ns "http://www.w3.org/2000/xmlns/")

(defn xml-tag-kw
  "Build a clojure.data.xml namespaced element tag."
  [uri local]
  (keyword (str "xmlns." (encode-xmlns-uri uri)) local))

(defn xsi-nil-attrs
  "Attribute map for xsi:nil=\"true\"."
  []
  {(xml-tag-kw xsi-ns "nil") "true"})

(defn adac-root-ns-attrs
  "Default ADAC xmlns + xmlns:xsi, matching Sample ADAC V6 / FME-friendly form."
  []
  {:xmlns adac-ns
   (xml-tag-kw xmlns-ns "xsi") xsi-ns})

(defn tag-local
  "Keyword or string tag → local element/attribute name."
  [tag]
  (cond
    (keyword? tag) (let [n (name tag)]
                       (if-let [i (str/last-index-of n \})]
                         (subs n (inc i))
                         n))
    (string? tag) (if-let [i (str/last-index-of tag ":")]
                    (subs tag (inc i))
                    tag)
    :else (str tag)))

(defn attr-local
  [attr-kw]
  (tag-local attr-kw))

(defn xsi-nil-attr?
  [k]
  (= "nil" (attr-local k)))

(defn xsi-nil?
  "True when element carries xsi:nil=\"true\"."
  [node]
  (some (fn [[k v]]
          (and (xsi-nil-attr? k)
               (or (= "true" (str/trim (str v)))
                   (true? v))))
        (:attrs node)))

(defn attrs-by-local
  [node]
  (into {}
        (map (fn [[k v]] [(attr-local k) (str v)]))
        (:attrs node)))

(defn element-children
  "Non-text content children of an XML element node."
  [node]
  (filter map? (:content node)))

(defn children-by-name
  "Group element children by local tag name → {name [nodes]}."
  [node]
  (reduce (fn [m child]
            (let [n (tag-local (:tag child))]
              (update m n (fnil conj []) child)))
          {}
          (element-children node)))

(defn text-content
  "Concatenate direct text content of a node."
  [node]
  (->> (:content node)
       (filter string?)
       (str/join)
       str/trim))

(defn parse-xml
  [source]
  (xml/parse source))

(defn element?
  [node]
  (and (map? node) (:tag node)))
