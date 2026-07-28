(ns adac-edn-converter.instance.schema-index
  "Lookup layer over a SchemaCraft schema bundle for instance XML I/O."
  (:require [clojure.string :as str]))

(defn build-index
  "Build index from {:schema :typedefs :elements} bundle."
  [bundle]
  (let [by-id (into {} (map (juxt :record/id identity) (:elements bundle)))
        by-name (group-by :record/name (:elements bundle))
        typedefs (into {} (map (juxt :record/id identity) (:typedefs bundle)))
        root-el (first (by-name :ADAC))]
    {:bundle bundle
     :by-id by-id
     :by-name by-name
     :typedefs typedefs
     :root root-el}))

(defn element
  [idx id]
  (get-in idx [:by-id id]))

(defn children
  "Ordered child Element records for a complex/host element."
  [idx el]
  (when el
    (mapv (fn [id] (element idx id))
          (or (:element/child-refs el) []))))

(defn typedef
  [idx id]
  (get-in idx [:typedefs id]))

(defn typedef-for-element
  [idx el]
  (when-let [tid (:element/typedef-id el)]
    (typedef idx tid)))

(defn parent-property?
  [el]
  (= :parent-property (:element/placement el)))

(defn property-name
  [el]
  (or (:element/property-name el)
      (name (:record/name el))))

(defn scalar?
  [el]
  (= :scalar (:element/kind el)))

(defn sequence?
  [el]
  (= :sequence (:element/kind el)))

(defn collection?
  [el]
  (= :collection (:element/kind el)))

(defn choice?
  [el]
  (= :choice (:element/kind el)))

(defn host-event?
  [el]
  (= :host-event (:element/kind el)))

(defn collection-item
  [idx el]
  (when-let [item-id (get-in el [:element/data :collection/item-ref])]
    (element idx item-id)))

(defn host-target
  [idx el]
  (when (host-event? el)
    (first (children idx el))))

(defn nillable?
  [el]
  (true? (:element/nillable? el)))

(defn fixed-value
  [el]
  (:element/fixed el))

(defn min-occurs
  [el]
  (get-in el [:element/cardinality :min] 0))

(defn element-key
  [el]
  (:element/key el))

(defn root-element
  [idx]
  (:root idx))
