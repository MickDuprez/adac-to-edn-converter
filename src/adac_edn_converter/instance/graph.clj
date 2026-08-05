(ns adac-edn-converter.instance.graph
  "Assembled Instance value maps ↔ SchemaCraft :instance-graph envelopes.

  SchemaCraft Document UI uses a synthetic :Document root whose children are
  top-level authored Elements (for ADAC: :ADAC). Structural sequences/choices
  and collection members are separate Instance rows; scalars, host-events, and
  instance-refs stay inline on the parent :instance/value."
  (:require [adac-edn-converter.instance.schema-index :as idx]
            [clojure.string :as str]))

(def ^:private nil-sentinel :schemacraft/nil)
(def document-root-key :Document)

(defn- new-id []
  (random-uuid))

(defn- explicit-nil? [v]
  (= nil-sentinel v))

(defn- present?
  [v]
  (cond
    (nil? v) false
    (explicit-nil? v) true
    (string? v) (not (str/blank? v))
    (map? v) (boolean (seq v))
    (sequential? v) (boolean (seq v))
    :else true))

(defn- collection-item-present?
  "Collection slots keep empty maps (e.g. [{}] from XML) as real members."
  [v]
  (cond
    (nil? v) false
    (explicit-nil? v) true
    (string? v) (not (str/blank? v))
    :else true))

(defn top-level-elements
  "Authored Elements not nested under another Element (IRE targets stay top-level)."
  [idx]
  (let [els (filterv #(= :element (:record/type %)) (vals (:by-id idx)))
        nested-ids (into #{}
                         (mapcat (fn [el]
                                   (when (not= :instance-ref (:element/type el))
                                     (:element/child-refs el))))
                         els)]
    (->> els
         (remove #(contains? nested-ids (:record/id %)))
         (sort-by (comp str :element/key))
         vec)))

(defn- choice-selected-key
  [idx choice-el value]
  (when (map? value)
    (or (:choice/selected value)
        (some (fn [alt]
                (let [k (idx/element-key alt)]
                  (when (contains? value k) k)))
              (idx/children idx choice-el)))))

(defn- inline-child?
  [el]
  (contains? #{:host-event :instance-ref} (:element/kind el)))

(defn- structural-child?
  [el]
  (contains? #{:sequence :choice} (:element/kind el)))

(declare emit-structural!)

(defn- parent-blob-value
  "Fields stored on this Instance row.

  Scalars / host-events / instance-refs are always inlined. Linked structural
  and collection children normally live as separate rows; we also keep
  `:schemacraft/nil` and empty `[]` markers on the parent so graph→assembled
  round-trips match XML import."
  [idx el value]
  (let [value (if (map? value) value {})]
    (if (idx/choice? el)
      (let [sel (choice-selected-key idx el value)]
        (cond-> {}
          sel (assoc :choice/selected sel)))
      (into {}
            (keep (fn [child]
                    (let [k (idx/element-key child)
                          v (get value k)]
                      (when (contains? value k)
                        (cond
                          (or (idx/scalar? child) (inline-child? child))
                          [k v]

                          (explicit-nil? v)
                          [k nil-sentinel]

                          (and (idx/collection? child)
                               (sequential? v)
                               (empty? v))
                          [k []]

                          :else nil)))))
            (idx/children idx el)))))

(defn- emit-row!
  [*rows row]
  (swap! *rows conj row)
  row)

(defn- emit-collection-items!
  [idx parent-id coll-el items *rows]
  (let [item-el (idx/collection-item idx coll-el)
        items (if (sequential? items) items [])]
    (doseq [[i item] (map-indexed vector items)]
      (when (and item-el (collection-item-present? item))
        (if (idx/scalar? item-el)
          (emit-row! *rows
                     {:instance/id (new-id)
                      :instance/element (idx/element-key item-el)
                      :instance/parent-id parent-id
                      :instance/member-key (idx/element-key coll-el)
                      :instance/member-index i
                      :instance/value {(idx/element-key item-el) item}})
          (emit-structural! idx item-el parent-id
                            (idx/element-key coll-el) i item *rows))))))

(defn- emit-structural!
  "Create one Instance for el and recurse into linked children."
  [idx el parent-id member-key member-index value *rows]
  (let [id (new-id)
        ;; Collection Elements store items as child rows; value is the item vector.
        coll-items (when (and (idx/collection? el) (sequential? value)) value)
        value (cond
                (explicit-nil? value) {}
                (map? value) value
                :else {})
        base-value (parent-blob-value idx el value)
        row (cond-> {:instance/id id
                     :instance/element (idx/element-key el)
                     :instance/value base-value}
              parent-id (assoc :instance/parent-id parent-id)
              member-key (assoc :instance/member-key member-key)
              (integer? member-index) (assoc :instance/member-index member-index))]
    (emit-row! *rows row)
    (when coll-items
      (emit-collection-items! idx id el coll-items *rows))
    (when (idx/choice? el)
      (when-let [sel (choice-selected-key idx el value)]
        (when-let [alt (some #(when (= sel (idx/element-key %)) %)
                             (idx/children idx el))]
          (when-let [branch (get value sel)]
            (cond
              (idx/collection? alt)
              (when (and (sequential? branch) (seq branch))
                (emit-collection-items! idx id alt branch *rows))

              (and (present? branch) (not (explicit-nil? branch)))
              (emit-structural! idx alt id sel nil branch *rows))))))
    (when (idx/sequence? el)
      (doseq [child (idx/children idx el)
              :let [k (idx/element-key child)
                    v (get value k)]
              :when (contains? value k)]
        (cond
          (or (idx/scalar? child) (inline-child? child))
          nil

          (explicit-nil? v)
          nil

          (idx/collection? child)
          (when (and (sequential? v) (seq v))
            (emit-collection-items! idx id child v *rows))

          (structural-child? child)
          (when (present? v)
            (emit-structural! idx child id k nil v *rows))

          :else
          (when (and (map? v) (present? v))
            (emit-structural! idx child id k nil v *rows)))))
    id))

(defn- document-label
  [assembled override]
  (or (not-empty (str/trim (str (or override ""))))
      (not-empty (str (get-in assembled [:Project :Name])))
      "ADAC document"))

(defn assembled->document-graph
  "Wrap an assembled ADAC body map as a SchemaCraft :document instance-graph.

  `assembled` is the value under :ADAC (what xml/read produces)."
  [bundle assembled & {:keys [label]}]
  (let [idx (idx/build-index bundle)
        schema (:schema bundle)
        schema-id (:record/id schema)
        top (top-level-elements idx)
        adac-el (or (first (filter #(= :ADAC (:record/name %)) top))
                    (idx/root-element idx)
                    (first top))
        root-id (new-id)
        *rows (atom [])
        doc-label (document-label assembled label)]
    (emit-row! *rows
               {:instance/id root-id
                :instance/element document-root-key
                :instance/root? true
                :instance/label doc-label
                :instance/value {}})
    (when adac-el
      (emit-structural! idx adac-el root-id (idx/element-key adac-el) nil
                        (or assembled {}) *rows))
    {:schemacraft/format :instance-graph
     :schemacraft/version 1
     :schemacraft/kind :document
     :schemacraft/exported-at (java.util.Date.)
     :schemacraft/schema
     {:id schema-id
      :name (str (:record/name schema))}
     :schemacraft/document {:id root-id :label doc-label}
     :schemacraft/instances (vec @*rows)}))

;;; ---------- graph → assembled ----------

(defn- instances-by-id
  [graph]
  (into {} (map (juxt :instance/id identity) (:schemacraft/instances graph))))

(defn- find-child
  ([by-id parent-id member-key]
   (find-child by-id parent-id member-key nil))
  ([by-id parent-id member-key member-index]
   (some (fn [inst]
           (when (and (= parent-id (:instance/parent-id inst))
                      (= member-key (:instance/member-key inst))
                      (= member-index (:instance/member-index inst)))
             inst))
         (vals by-id))))

(defn- collection-items
  [by-id parent-id member-key]
  (->> (vals by-id)
       (filter #(and (= parent-id (:instance/parent-id %))
                     (= member-key (:instance/member-key %))
                     (integer? (:instance/member-index %))))
       (sort-by :instance/member-index)
       vec))

(defn- assemble-node
  [idx by-id el inst]
  (when inst
    (let [base (or (:instance/value inst) {})]
      (cond
        (idx/collection? el)
        (let [item-el (idx/collection-item idx el)
              items (collection-items by-id (:instance/id inst) (idx/element-key el))]
          (mapv (fn [item-inst]
                  (cond
                    (nil? item-el) {}
                    (idx/scalar? item-el)
                    (get (:instance/value item-inst) (idx/element-key item-el))
                    (idx/choice? item-el)
                    (assemble-node idx by-id item-el item-inst)
                    (idx/collection? item-el)
                    (assemble-node idx by-id item-el item-inst)
                    :else
                    (assemble-node idx by-id item-el item-inst)))
                items))

        (idx/choice? el)
        (let [sel (:choice/selected base)
              alt (when sel
                    (some #(when (= sel (idx/element-key %)) %)
                          (idx/children idx el)))]
          (cond
            (and alt (idx/collection? alt))
            (let [items (collection-items by-id (:instance/id inst) sel)
                  item-el (idx/collection-item idx alt)]
              (cond-> {}
                sel (assoc :choice/selected sel)
                (seq items)
                (assoc sel
                       (mapv (fn [item-inst]
                               (if (and item-el (idx/scalar? item-el))
                                 (get (:instance/value item-inst)
                                      (idx/element-key item-el))
                                 (assemble-node idx by-id item-el item-inst)))
                             items))))

            :else
            (let [child (when sel (find-child by-id (:instance/id inst) sel nil))]
              (cond-> {}
                sel (assoc :choice/selected sel)
                (and alt child) (assoc sel (assemble-node idx by-id alt child))))))

        :else
        (let [from-children
              (reduce
               (fn [acc child]
                 (let [k (idx/element-key child)]
                   (cond
                     (or (idx/scalar? child) (inline-child? child))
                     (if (contains? base k)
                       (assoc acc k (get base k))
                       acc)

                     (idx/collection? child)
                     (let [item-el (idx/collection-item idx child)
                           items (collection-items by-id (:instance/id inst) k)]
                       (cond
                         (seq items)
                         (assoc acc k
                                (mapv (fn [item-inst]
                                        (cond
                                          (and item-el (idx/scalar? item-el))
                                          (get (:instance/value item-inst)
                                               (idx/element-key item-el))
                                          (and item-el (idx/collection? item-el))
                                          (assemble-node idx by-id item-el item-inst)
                                          :else
                                          (assemble-node idx by-id item-el item-inst)))
                                      items))

                         (contains? base k)
                         (assoc acc k (get base k))

                         :else acc))

                     (structural-child? child)
                     (if-let [c (find-child by-id (:instance/id inst) k nil)]
                       (assoc acc k (assemble-node idx by-id child c))
                       (if (contains? base k)
                         (assoc acc k (get base k))
                         acc))

                     :else acc)))
               {}
               (idx/children idx el))
              linked-keys (into #{}
                                (comp (filter #(or (structural-child? %)
                                                   (idx/collection? %)))
                                      (map idx/element-key))
                                (idx/children idx el))
              linked-with-rows
              (into #{}
                    (filter (fn [k]
                              (or (seq (collection-items by-id (:instance/id inst) k))
                                  (some? (find-child by-id (:instance/id inst) k nil)))))
                    linked-keys)
              from-base (apply dissoc base linked-with-rows)]
          (merge from-base from-children))))))

(defn graph->assembled
  "Collapse a :document instance-graph to the assembled ADAC body map."
  [bundle graph]
  (let [idx (idx/build-index bundle)
        by-id (instances-by-id graph)
        root (or (first (filter :instance/root? (vals by-id)))
                 (get by-id (get-in graph [:schemacraft/document :id])))
        adac-el (idx/root-element idx)
        adac-key (when adac-el (idx/element-key adac-el))
        adac-inst (when (and root adac-key)
                    (find-child by-id (:instance/id root) adac-key nil))]
    (if (and adac-el adac-inst)
      (assemble-node idx by-id adac-el adac-inst)
      {})))

(defn instance-graph?
  [data]
  (and (map? data) (= :instance-graph (:schemacraft/format data))))

(defn coerce-assembled
  "If data is an instance-graph, collapse to assembled ADAC body; else return as-is."
  [bundle data]
  (if (instance-graph? data)
    (graph->assembled bundle data)
    data))
