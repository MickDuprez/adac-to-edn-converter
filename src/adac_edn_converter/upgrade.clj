(ns adac-edn-converter.upgrade
  "Assembled ADAC v5.0.1 → v6.0.0 upgrade (same-name pass-through + diff table)."
  (:require [adac-edn-converter.instance.schema-index :as idx]
            [clojure.string :as str]
            [clojure.walk :as walk]))

(def ^:private nil-sentinel :schemacraft/nil)

;;; ---------- leaf key renames ----------

(defn- sqm->m2
  "Rename Area*_sqm / *_sqm leaf keys to *_m2."
  [k]
  (let [n (name k)]
    (if (str/ends-with? n "_sqm")
      (keyword (str (subs n 0 (- (count n) 4)) "_m2"))
      k)))

(def ^:private exact-renames
  "Flat local-name renames (v5 → v6). Diameter_mm→Size_mm is NOT global —
  it applies only under Electrical conduits (see conduit-field-renames)."
  {:ElectricalConduits :Conduits
   :ElectricalConduit :Conduit
   :ElectricalFittings :Fittings
   :ElectricalFitting :Fitting})

(def ^:private conduit-field-renames
  "Electrical Conduit field renames only (v5 Diameter_mm → v6 Size_mm)."
  {:Diameter_mm :Size_mm})

(defn- rename-key
  [k]
  (let [k' (get exact-renames k k)]
    (sqm->m2 k')))

(defn- rename-conduit-fields
  "Rename Diameter_mm→Size_mm inside a Conduit item or Conduits collection."
  [v]
  (cond
    (map? v)
    (reduce-kv (fn [acc k child]
                 (let [k' (get conduit-field-renames k k)]
                   (assoc acc k' (rename-conduit-fields child))))
               {}
               v)

    (vector? v)
    (mapv rename-conduit-fields v)

    :else v))

;;; ---------- structural relocates (before unknown-key drop) ----------

(defn- rename-keys-deep
  "Recursively rename map keys via rename-key (vectors preserved)."
  [v]
  (cond
    (map? v)
    (reduce-kv (fn [acc k child]
                 (assoc acc (rename-key k) (rename-keys-deep child)))
               {}
               v)

    (vector? v)
    (mapv rename-keys-deep v)

    :else v))

(defn- outlet-protection-from-v5
  "Best-effort WingWall/Apron → OutletProtection {Material Area_m2}."
  [end-struct]
  (when (map? end-struct)
    (let [apron (get end-struct :Apron)
          material (or (get-in apron [:Apron_Material])
                       (get-in end-struct [:WingWall :LWW_Material])
                       (get-in end-struct [:WingWall :RWW_Material]))
          area (or (get apron :Apron_Area_m2)
                   (get apron :Apron_Area_sqm))]
      (when (or material area)
        (cond-> {}
          material (assoc :Material material)
          area (assoc :Area_m2 area))))))

(defn- upgrade-end-structure
  "Replace WingWall/Apron with OutletProtection when present."
  [end-struct]
  (if-not (map? end-struct)
    end-struct
    (let [op (or (get end-struct :OutletProtection)
                 (outlet-protection-from-v5 end-struct))]
      (cond-> (dissoc end-struct :WingWall :Apron)
        op (assoc :OutletProtection op)))))

(defn- walk-end-structures
  "Any map that still carries WingWall/Apron becomes OutletProtection-shaped."
  [m]
  (walk/postwalk
   (fn [x]
     (if (and (map? x) (or (contains? x :WingWall) (contains? x :Apron)))
       (upgrade-end-structure x)
       x))
   m))

(defn- relocate-openspace-electrical
  "Move OpenSpace ElectricalConduits/Fittings → ProjectData Electrical.

  Applies conduit-only Diameter_mm→Size_mm after collection/item renames."
  [m]
  (let [pd-path [:Project :ProjectData]
        pd (get-in m pd-path)]
    (if-not (map? pd)
      m
      (let [os (get pd :OpenSpace)
            conduits (get os :ElectricalConduits)
            fittings (get os :ElectricalFittings)
            os' (cond-> (or os {})
                  true (dissoc :ElectricalConduits :ElectricalFittings))
            conduits' (when (some? conduits)
                        (-> conduits
                            rename-keys-deep
                            rename-conduit-fields))
            fittings' (when (some? fittings)
                        (rename-keys-deep fittings))
            electrical (cond-> (or (get pd :Electrical) {})
                         (some? conduits') (assoc :Conduits conduits')
                         (some? fittings') (assoc :Fittings fittings'))
            pd' (cond-> (assoc pd :OpenSpace os')
                  (seq electrical) (assoc :Electrical electrical))]
        (assoc-in m pd-path pd')))))

(defn- preprocess-v5
  "Apply structural relocates and deep key renames before schema filter."
  [assembled]
  (-> (or assembled {})
      relocate-openspace-electrical
      walk-end-structures
      rename-keys-deep
      (assoc :version "6.0.0")))

;;; ---------- enum fallbacks ----------

(defn- enum-items
  [td]
  (or (get-in td [:typedef/facets :items])
      (get-in td [:typedef/facets :enumeration])
      (get-in td [:element/data :type/facets :items])
      []))

(defn- enum-fallback
  "Prefer Unknown, then Other, when present on the typedef."
  [td]
  (let [items (set (map str (enum-items td)))]
    (cond
      (contains? items "Unknown") "Unknown"
      (contains? items "Other") "Other"
      :else nil)))

(defn- coerce-enum-leaf
  "If value is a string not in the v6 enum, substitute Unknown/Other when available."
  [td v]
  (if-not (and td (= :enum (:typedef/primitive td)) (string? v))
    v
    (let [items (set (map str (enum-items td)))
          s (str v)]
      (if (or (empty? items) (contains? items s))
        v
        (or (enum-fallback td) v)))))

;;; ---------- recursive filter / enum pass against v6 schema ----------

(defn- known-child-keys
  [v6-idx el]
  (into #{} (map idx/element-key) (idx/children v6-idx el)))

(defn- find-child-by-key
  [v6-idx el k]
  (some #(when (= k (idx/element-key %)) %) (idx/children v6-idx el)))

(defn- choice-alt-keys
  [v6-idx choice-el]
  (into #{} (map idx/element-key) (idx/children v6-idx choice-el)))

(defn- adapt-choice-collection-value
  "v5 Ring/Path is often a sequence map {:PolySegment …}; v6 is a collection
  of Ring_Fragment/Path_Fragment choices. Wrap a lone alt map into a vector."
  [v6-idx coll-el v]
  (let [item-el (when (and coll-el (idx/collection? coll-el))
                  (idx/collection-item v6-idx coll-el))]
    (if-not (and item-el (idx/choice? item-el) (map? v)
                 (not (vector? v)))
      v
      (let [alts (choice-alt-keys v6-idx item-el)
            sel (or (:choice/selected v)
                    (some #(when (contains? v %) %) alts))]
        (if sel
          [(cond-> (dissoc v :choice/selected)
             true (assoc :choice/selected sel)
             (contains? v sel) identity)]
          v)))))

(declare filter-value)

(defn- filter-map
  [v6-idx parent-el m]
  (let [allowed (when parent-el (known-child-keys v6-idx parent-el))]
    (reduce-kv
     (fn [acc k v]
       (let [keep? (or (nil? allowed)
                       (contains? allowed k)
                       (= k :version)
                       (= k :choice/selected))]
         (if-not keep?
           acc
           (let [child (when parent-el (find-child-by-key v6-idx parent-el k))
                 v' (filter-value v6-idx child v)]
             (assoc acc k v')))))
     {}
     m)))

(defn- filter-value
  [v6-idx el v]
  (cond
    (nil? v) nil
    (= nil-sentinel v) nil-sentinel

    (and el (idx/scalar? el))
    (coerce-enum-leaf (idx/typedef-for-element v6-idx el) v)

    (and el (idx/collection? el))
    (let [v (adapt-choice-collection-value v6-idx el v)
          item-el (idx/collection-item v6-idx el)
          items (cond
                  (nil? v) []
                  (vector? v) v
                  :else [v])]
      (mapv #(filter-value v6-idx item-el %) items))

    (vector? v)
    (let [item-el (when (and el (idx/collection? el))
                    (idx/collection-item v6-idx el))]
      (mapv #(filter-value v6-idx item-el %) v))

    (map? v)
    (cond
      (and el (idx/choice? el))
      (let [sel (or (:choice/selected v)
                    (some (fn [alt]
                            (let [ak (idx/element-key alt)]
                              (when (contains? v ak) ak)))
                          (idx/children v6-idx el)))
            body (dissoc v :choice/selected)
            ;; Filter the selected alt's body against the alt element, not the
            ;; choice wrapper (choice children are alts, not fields of the map).
            alt (when sel (find-child-by-key v6-idx el sel))
            upgraded (if alt
                       (let [alt-body (get body sel)
                             alt' (filter-value v6-idx alt alt-body)]
                         (cond-> {}
                           (some? alt') (assoc sel alt')))
                       (filter-map v6-idx el body))]
        (cond-> upgraded
          sel (assoc :choice/selected sel)))

      el
      (filter-map v6-idx el v)

      :else
      (filter-map v6-idx nil v))

    :else v))

(defn upgrade-v501->v600
  "Upgrade an assembled ADAC body parsed with the v5.0.1 bundle toward v6.0.0.

  - Sets root :version to \"6.0.0\"
  - Relocates OpenSpace electrical → Electrical; WingWall/Apron → OutletProtection
  - Passes through same-named keys present on the v6 schema
  - Applies *_sqm→*_m2 and the rename table (Diameter_mm→Size_mm only on conduits)
  - Adapts v5 Ring/Path sequence maps to v6 collection-of-choice vectors
  - Enum values missing from v6 → \"Unknown\" / \"Other\" when available
  - Drops keys unknown to v6

  Remaining gaps are left for import-document! soft-deferred / :schemacraft/nil."
  [v600-bundle assembled-v5]
  (let [ix (idx/build-index v600-bundle)
        root (idx/root-element ix)
        prepared (preprocess-v5 assembled-v5)
        upgraded (filter-value ix root prepared)]
    (assoc (if (map? upgraded) upgraded {}) :version "6.0.0")))
