(ns adac-edn-converter.bcib.convert
  "BCIB domain (clients → assets/surveys/renewals) + survey forms → SchemaCraft."
  (:require [adac-edn-converter.util :as u]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def schema-id
  (u/stable-uuid "schema/BCIB"))

(def default-legacy-path
  "doc/bcib/bcib-schema.edn")

(def default-overlay-path
  "doc/bcib/behaviour-overlay.csv")

;;; ---------- CSV / load ----------

(defn- parse-csv-line
  [line]
  (loop [s line, acc [], cur nil, in-q false]
    (if (empty? s)
      (conj acc (or cur ""))
      (let [c (first s)]
        (cond
          (= c \")
          (if (and in-q (= (second s) \"))
            (recur (nnext s) acc (str cur \") true)
            (recur (next s) acc (or cur "") (not in-q)))
          (and (= c \,) (not in-q))
          (recur (next s) (conj acc (or cur "")) nil false)
          :else
          (recur (next s) acc (str cur c) in-q))))))

(defn load-overlay
  "Parse behaviour-overlay.csv → map of question_id (long) → row map."
  [path]
  (with-open [r (io/reader path)]
    (let [lines (doall (line-seq r))
          hdr (parse-csv-line (first lines))]
      (into {}
            (keep (fn [line]
                    (when-not (str/blank? line)
                      (let [row (zipmap hdr (parse-csv-line line))
                            id (try (Long/parseLong (str/trim (str (get row "question_id"))))
                                    (catch Exception _ nil))]
                        (when id [id row]))))
                  (rest lines))))))

(defn load-legacy
  [path]
  (edn/read-string (slurp path)))

;;; ---------- naming ----------

(defn- slug
  "Label → PascalCase identifier token. Apostrophes are dropped so
  \"Greenkeeper's Shed\" → GreenkeepersShed (not GreenkeeperSShed)."
  [label]
  (let [cleaned (-> (str label)
                    (str/replace #"['’]" "")
                    (str/replace #"[^A-Za-z0-9]+" " "))
        parts (->> (str/split cleaned #"\s+")
                   (remove str/blank?)
                   (map str/capitalize))]
    (if (seq parts)
      (str/join parts)
      "Untitled")))

(defn- source-id-from-kw
  "`:bcib.form/id-3` → 3"
  [kw]
  (when-let [n (and kw (name kw))]
    (when-let [m (re-find #"id-(\d+)$" n)]
      (Long/parseLong (second m)))))

(defn- field-source-id
  [field]
  (or (:source/id field)
      (source-id-from-kw (:field/id field))))

(defn- q-name
  [id]
  (keyword (str "Q" id)))

;;; ---------- typedefs ----------

(defn- make-typedef
  [name label primitive facets]
  (let [kw (u/as-keyword name)
        id (u/stable-uuid (str "bcib/typedef/" name))]
    {:record/type :typedef
     :record/id id
     :schema/id schema-id
     :record/parent-id schema-id
     :record/name kw
     :record/label label
     :record/documentation nil
     :typedef/primitive primitive
     :typedef/facets facets
     :element/key kw
     :element/kind :typedef
     :element/label label
     :element/documentation nil
     :element/cardinality {:min 0 :max 1}
     :element/children []
     :element/data {:type/primitive primitive
                    :type/facets facets}}))

(defn- shared-typedefs
  []
  {:string (make-typedef "BCIB_String" "BCIB String" :string {:trim? true})
   :decimal (make-typedef "BCIB_Decimal" "BCIB Decimal" :decimal {})
   :money (make-typedef "BCIB_Money" "BCIB Money" :decimal {:fraction-digits 2})
   :boolean (make-typedef "BCIB_Boolean" "BCIB Boolean" :boolean {})
   :date (make-typedef "BCIB_Date" "BCIB Date" :date {})})

(defn- options-fingerprint
  [options]
  (->> (or options [])
       (map str)
       (str/join "\u0001")))

(defn- enum-typedef
  [options]
  (let [items (mapv str options)
        fp (options-fingerprint items)
        short-hash (subs (str (u/stable-uuid (str "bcib/enum-hash/" fp))) 0 8)
        name (str "Enum_" short-hash)
        facets {:base :string :items items}]
    (make-typedef name (str "Enum (" (count items) " items)") :enum facets)))

;;; ---------- elements ----------

(defn- cardinality
  [required?]
  {:min (if required? 1 0) :max 1})

(defn- make-sequence
  [{:keys [name label documentation child-refs order]}]
  (let [kw (u/as-keyword name)
        id (u/stable-uuid (str "bcib/element/" name))
        doc (when-not (str/blank? documentation) documentation)]
    {:record/type :element
     :record/id id
     :schema/id schema-id
     :record/parent-id schema-id
     :record/name kw
     :record/label (or label name)
     :record/documentation doc
     :element/type :complex
     :element/key kw
     :element/kind :sequence
     :element/label (or label name)
     :element/documentation doc
     :element/typedef-id nil
     :element/child-refs (vec child-refs)
     :element/cardinality {:min 0 :max 1}
     :element/order (or order 0)
     :element/children []
     :element/status :active
     :element/data {:child-refs (vec child-refs)}}))

(defn- make-scalar
  "Survey Q-fields (default) or domain scalars when :id-key is provided."
  [{:keys [name label documentation typedef-id required? order behaviour id-key]}]
  (let [kw (u/as-keyword name)
        local (if (keyword? name) (clojure.core/name name) (str name))
        id (cond
             id-key (u/stable-uuid id-key)
             (re-find #"^Q(\d+)$" local)
             (u/stable-uuid (str "bcib/field/" (second (re-find #"^Q(\d+)$" local))))
             :else (u/stable-uuid (str "bcib/field/" local)))
        doc (when-not (str/blank? documentation) documentation)
        nillable? (not required?)
        data (cond-> {:type/ref typedef-id}
               nillable? (assoc :nillable? true)
               (seq behaviour) (assoc :bcib/behaviour behaviour))]
    (cond-> {:record/type :element
             :record/id id
             :schema/id schema-id
             :record/parent-id schema-id
             :record/name kw
             :record/label (or label local)
             :record/documentation doc
             :element/type :simple
             :element/key kw
             :element/kind :scalar
             :element/label (or label local)
             :element/documentation doc
             :element/typedef-id typedef-id
             :element/child-refs nil
             :element/cardinality (cardinality required?)
             :element/order (or order 0)
             :element/children []
             :element/status :active
             :element/data data}
      nillable? (assoc :element/nillable? true))))

(defn- make-collection
  [{:keys [name label documentation item-ref order]}]
  (let [kw (u/as-keyword name)
        id (u/stable-uuid (str "bcib/element/" (clojure.core/name kw)))
        doc (when-not (str/blank? documentation) documentation)
        child-refs [item-ref]]
    {:record/type :element
     :record/id id
     :schema/id schema-id
     :record/parent-id schema-id
     :record/name kw
     :record/label (or label (clojure.core/name kw))
     :record/documentation doc
     :element/type :complex
     :element/key kw
     :element/kind :collection
     :element/label (or label (clojure.core/name kw))
     :element/documentation doc
     :element/typedef-id nil
     :element/child-refs child-refs
     :element/cardinality {:min 0 :max :many}
     :element/order (or order 0)
     :element/children []
     :element/status :active
     :element/data {:child-refs child-refs
                    :collection/item-ref item-ref}}))

(defn- make-instance-ref
  [{:keys [name label documentation target-id required? order]}]
  (let [kw (u/as-keyword name)
        local (clojure.core/name kw)
        id (u/stable-uuid (str "bcib/ire/" local))
        doc (when-not (str/blank? documentation) documentation)
        child-refs [target-id]
        ire {:target-id target-id}]
    {:record/type :element
     :record/id id
     :schema/id schema-id
     :record/parent-id schema-id
     :record/name kw
     :record/label (or label local)
     :record/documentation doc
     :element/type :instance-ref
     :element/key kw
     :element/kind :instance-ref
     :element/label (or label local)
     :element/documentation doc
     :element/typedef-id nil
     :element/child-refs child-refs
     :element/instance-ref ire
     :element/cardinality (cardinality required?)
     :element/order (or order 0)
     :element/children []
     :element/status :active
     :element/data {:child-refs child-refs
                    :instance-ref ire}}))

(defn- make-choice
  "Pick one of the alternative Elements (SchemaCraft :choice)."
  [{:keys [name label documentation child-refs order required?]}]
  (let [kw (u/as-keyword name)
        local (clojure.core/name kw)
        id (u/stable-uuid (str "bcib/element/" local))
        doc (when-not (str/blank? documentation) documentation)
        refs (vec child-refs)]
    {:record/type :element
     :record/id id
     :schema/id schema-id
     :record/parent-id schema-id
     :record/name kw
     :record/label (or label local)
     :record/documentation doc
     :element/type :complex
     :element/key kw
     :element/kind :choice
     :element/label (or label local)
     :element/documentation doc
     :element/typedef-id nil
     :element/child-refs refs
     :element/cardinality (cardinality (boolean required?))
     :element/order (or order 0)
     :element/children []
     :element/status :active
     :element/data {:child-refs refs}}))

(defn- domain-scalar
  [shared owner {:keys [name label typedef required? order]}]
  (let [local (if (keyword? name) (clojure.core/name name) (str name))
        td (get shared (or typedef :string))]
    (make-scalar
     {:name name
      :label (or label local)
      :typedef-id (:record/id td)
      :required? (boolean required?)
      :order (or order 0)
      :id-key (str "bcib/domain/" owner "/" local)})))

;;; ---------- behaviour ----------

(defn- blank-cell?
  [s]
  (or (nil? s) (str/blank? (str s))))

(defn- parse-field-ref
  "`:bcib.question/id-152` or keyword → long id"
  [x]
  (cond
    (number? x) (long x)
    (keyword? x) (source-id-from-kw x)
    (string? x) (try (Long/parseLong x) (catch Exception _ (source-id-from-kw (keyword x))))
    :else nil))

(defn- normalize-calc
  [field]
  (let [calc (:calc field)
        top-fields (:fields field)]
    (when (or calc top-fields)
      (let [op (or (:op calc) :sum)
            refs (or (:fields calc) top-fields [])
            ids (vec (keep parse-field-ref refs))]
        {:op op :source-field-ids ids}))))

(defn- overlay-behaviour
  [overlay-row]
  (when overlay-row
    (let [show (get overlay-row "show_when_question_id")
          opt (get overlay-row "optional_section")
          calc-op (get overlay-row "calc_op")
          calc-fields (get overlay-row "calc_fields")]
      (cond-> {}
        (not (blank-cell? show))
        (assoc :show-when {:source-question-id (Long/parseLong (str/trim (str show)))})
        (and (not (blank-cell? opt))
             (#{"yes" "true" "1"} (str/lower-case (str/trim (str opt)))))
        (assoc :optional-section? true)
        (not (blank-cell? calc-op))
        (assoc :calc {:op (keyword (str/lower-case (str/trim (str calc-op))))
                      :source-field-ids
                      (if (blank-cell? calc-fields)
                        []
                        (->> (str/split (str calc-fields) #"[;,\s]+")
                             (remove str/blank?)
                             (map #(Long/parseLong %))
                             vec))})))))

(defn- merge-behaviour
  [field overlay-row]
  (let [from-overlay (overlay-behaviour overlay-row)
        from-legacy (when-let [c (normalize-calc field)]
                      {:calc c})
        calc (let [oc (:calc from-overlay)
                   lc (:calc from-legacy)]
               (cond
                 (and oc (seq (:source-field-ids oc))) oc
                 (and oc lc) (assoc oc :source-field-ids (:source-field-ids lc))
                 oc oc
                 lc lc
                 :else nil))
        merged (cond-> (merge (dissoc from-legacy :calc) (dissoc from-overlay :calc))
                 calc (assoc :calc calc))]
    (when (seq merged) merged)))

(defn- resolve-behaviour-uuids
  "Replace :source-field-ids / :show-when :source-question-id with :field-id UUIDs."
  [behaviour field-id-by-source]
  (when (seq behaviour)
    (cond-> behaviour
      (:show-when behaviour)
      (update :show-when
              (fn [sw]
                (let [sid (:source-question-id sw)]
                  (cond-> sw
                    (get field-id-by-source sid)
                    (assoc :field-id (get field-id-by-source sid))))))
      (:calc behaviour)
      (update :calc
              (fn [c]
                (let [ids (:source-field-ids c)
                      uuids (vec (keep field-id-by-source ids))]
                  (-> c
                      (assoc :field-ids uuids)
                      (dissoc :source-field-ids))))))))

;;; ---------- convert ----------

(defn- typedef-for-field
  [shared enum-cache field]
  (case (:ui/control field)
    :select
    (let [fp (options-fingerprint (:options field))]
      (or (get @enum-cache fp)
          (let [td (enum-typedef (:options field))]
            (swap! enum-cache assoc fp td)
            td)))
    :checkbox (:boolean shared)
    :number (:decimal shared)
    :currency (:money shared)
    :textarea (:string shared)
    ;; fallback
    (:string shared)))

(defn- emit-schema-root
  [legacy]
  (let [label (or (:schema/name legacy) "BCIB")
        doc "BCIB renewals domain + survey forms (clients, assets, renewals, lookups)."]
    {:record/type :schema
     :record/id schema-id
     :schema/id schema-id
     :record/parent-id nil
     :record/name "BCIB"
     :record/label label
     :record/documentation doc
     :schema/version 1.0M
     :schema/status :draft
     :element/key :Schema
     :element/kind :schema
     :element/label label
     :element/documentation doc
     :element/cardinality {:min 1 :max 1}
     :element/children []
     :element/data {}}))

(defn- emit-lookup
  "Lookup catalogue: Item sequence + Item_list collection. Name is first
  scalar for IRE dropdown labels."
  [shared {:keys [item-name list-name label fields]}]
  (let [scalars
        (mapv (fn [f i]
                (domain-scalar shared item-name (assoc f :order (inc i))))
              fields
              (range))
        item (make-sequence
              {:name item-name
               :label label
               :documentation nil
               :child-refs (mapv :record/id scalars)
               :order 0})
        coll (make-collection
              {:name list-name
               :label (str label " list")
               :item-ref (:record/id item)
               :order 0})]
    {:item item :list coll :scalars scalars}))

(defn- emit-domain
  "Client-rooted domain + lookup catalogues. Surveys nest under RenewalProgram;
  Asset is cross-year under Client and referenced from Survey via IRE."
  [shared form-els]
  (let [am (emit-lookup
            shared
            {:item-name "AccountManager"
             :list-name "AccountManager_list"
             :label "Account manager"
             :fields [{:name :Name :required? false}
                      {:name :Initials :required? false}
                      {:name :IsActive :typedef :boolean :required? true}]})
        area (emit-lookup
              shared
              {:item-name "Area"
               :list-name "Area_list"
               :label "Area"
               :fields [{:name :Name :required? false}]})
        state (emit-lookup
               shared
               {:item-name "State"
                :list-name "State_list"
                :label "State"
                :fields [{:name :Name :required? false}]})
        plan (emit-lookup
              shared
              {:item-name "ServicePlan"
               :list-name "ServicePlan_list"
               :label "Service plan"
               :fields [{:name :Name :required? false}
                        {:name :AbbreviatedName :required? false}
                        {:name :IsActive :typedef :boolean :required? true}]})
        team (emit-lookup
              shared
              {:item-name "ServiceTeam"
               :list-name "ServiceTeam_list"
               :label "Service team"
               :fields [{:name :Name :required? false}
                        {:name :Initials :required? false}
                        {:name :IsActive :typedef :boolean :required? true}]})
        am-id (get-in am [:item :record/id])
        area-id (get-in area [:item :record/id])
        state-id (get-in state [:item :record/id])
        plan-id (get-in plan [:item :record/id])
        team-id (get-in team [:item :record/id])

        ;; Assets live under Client (cross-year); surveys reference them via IRE.
        asset-scalars
        [(domain-scalar shared "Asset" {:name :Name :required? true :order 1})
         (domain-scalar shared "Asset" {:name :Description :required? true :order 2})]
        asset
        (make-sequence
         {:name "Asset"
          :label "Asset"
          :child-refs (mapv :record/id asset-scalars)
          :order 0})
        asset-list
        (make-collection
         {:name "Asset_list"
          :label "Assets"
          :item-ref (:record/id asset)
          :order 0})
        asset-ire
        (make-instance-ref
         {:name :Asset
          :label "Asset"
          :target-id (:record/id asset)
          :required? false
          :order 4})

        survey-scalars
        [(domain-scalar shared "Survey" {:name :Name :required? true :order 1})
         (domain-scalar shared "Survey" {:name :Description :required? false :order 2})]
        form-ids (mapv :record/id form-els)
        survey-form
        (make-choice
         {:name "SurveyForm"
          :label "Survey form"
          :documentation "Pick one survey template for this Survey Instance."
          :child-refs form-ids
          :required? false
          :order 5})
        survey
        (make-sequence
         {:name "Survey"
          :label "Survey"
          :child-refs (vec (concat (mapv :record/id survey-scalars)
                                   [(:record/id asset-ire)
                                    (:record/id survey-form)]))
          :order 0})
        survey-list
        (make-collection
         {:name "Survey_list"
          :label "Surveys"
          :item-ref (:record/id survey)
          :order 0})

        rp-scalars
        [(domain-scalar shared "RenewalProgram" {:name :Name :required? true :order 1})
         (domain-scalar shared "RenewalProgram" {:name :Description :required? true :order 2})
         (domain-scalar shared "RenewalProgram" {:name :DueDate :typedef :date :required? true :order 3})]
        renewal-program
        (make-sequence
         {:name "RenewalProgram"
          :label "Renewal program"
          :child-refs (conj (mapv :record/id rp-scalars)
                            (:record/id survey-list))
          :order 0})

        renewal-scalars
        [(domain-scalar shared "Renewal" {:name :Name :required? true :order 1})
         (domain-scalar shared "Renewal" {:name :RenewalDate :typedef :date :required? true :order 2})
         (domain-scalar shared "Renewal" {:name :IsOpen :typedef :boolean :required? true :order 3})]
        renewal
        (make-sequence
         {:name "Renewal"
          :label "Renewal"
          :child-refs (conj (mapv :record/id renewal-scalars)
                            (:record/id renewal-program))
          :order 0})
        renewal-list
        (make-collection
         {:name "Renewal_list"
          :label "Renewals"
          :item-ref (:record/id renewal)
          :order 0})

        client-scalar-specs
        [{:name :InsuredName :required? true}
         {:name :TradingName}
         {:name :Abn :label "ABN"}
         {:name :BusinessDescription}
         {:name :InterestedParties}
         {:name :MainContact}
         {:name :MainContactPhone}
         {:name :MainContactPosition}
         {:name :MainEmail}
         {:name :MainPhone}
         {:name :OtherContact}
         {:name :OtherContactPhone}
         {:name :OtherContactPosition}
         {:name :OccupancyArrangements}
         {:name :OperatingDays}
         {:name :PeriodOfInsurance}
         {:name :PostalAddress1}
         {:name :PostalAddress2}
         {:name :PostalAddress3}
         {:name :SituationAddress1}
         {:name :SituationAddress2}
         {:name :SituationAddress3}
         {:name :Sponsorship}
         {:name :WebUrl}
         {:name :YearEstablished}]
        client-scalars
        (mapv (fn [spec i]
                (domain-scalar shared "Client" (assoc spec :order (+ 10 i))))
              client-scalar-specs
              (range))

        client-ire-specs
        [{:name :AccountManager :target am-id}
         {:name :Area :target area-id}
         {:name :State :target state-id}
         {:name :ServicePlan :target plan-id}
         {:name :ServiceTeam :target team-id}
         {:name :ItcPercentage :target team-id :label "ITC percentage"}
         {:name :LocationDescription :target team-id}
         {:name :RiskLocation :target team-id}
         {:name :StampDutyExempt :target team-id}
         {:name :SurveyWith :target team-id}]
        client-ires
        (mapv (fn [spec i]
                (make-instance-ref
                 {:name (:name spec)
                  :label (:label spec)
                  :target-id (:target spec)
                  :required? false
                  :order (+ 100 i)}))
              client-ire-specs
              (range))

        client
        (make-sequence
         {:name "Client"
          :label "Client"
          :child-refs (vec (concat (mapv :record/id client-scalars)
                                   (mapv :record/id client-ires)
                                   [(:record/id asset-list)
                                    (:record/id renewal-list)]))
          :order 0})
        client-list
        (make-collection
         {:name "Client_list"
          :label "Clients"
          :item-ref (:record/id client)
          :order 0})

        top-lists [client-list
                   (:list am) (:list area) (:list state) (:list plan) (:list team)]
        all-els (vec
                 (concat
                  top-lists
                  [(:item am) (:item area) (:item state) (:item plan) (:item team)]
                  (:scalars am) (:scalars area) (:scalars state)
                  (:scalars plan) (:scalars team)
                  [client]
                  client-scalars
                  client-ires
                  [asset-list asset]
                  asset-scalars
                  [renewal-list renewal]
                  renewal-scalars
                  [renewal-program]
                  rp-scalars
                  [survey-list survey asset-ire survey-form]
                  survey-scalars))]
    {:top-lists top-lists
     :elements all-els
     :client client
     :survey survey}))

(defn convert
  "Convert legacy BCIB map + overlay → SchemaCraft {:schema :typedefs :elements}."
  [legacy overlay]
  (let [shared (shared-typedefs)
        enum-cache (atom {})
        fields (:fields legacy)
        qsets (:question-sets legacy)
        forms (:forms legacy)
        field-els
        (into {}
              (map (fn [[_ field]]
                     (let [sid (field-source-id field)
                           overlay-row (get overlay sid)
                           behaviour0 (merge-behaviour field overlay-row)
                           td (typedef-for-field shared enum-cache field)
                           required? (true? (get-in field [:validation :required]))
                           el (make-scalar
                               {:name (q-name sid)
                                :label (:label field)
                                :documentation (:help field)
                                :typedef-id (:record/id td)
                                :required? required?
                                :order (or sid 0)
                                :behaviour behaviour0})]
                       [sid el]))
                   fields))
        field-id-by-source (into {} (map (fn [[sid el]] [sid (:record/id el)]) field-els))
        field-els
        (into {}
              (map (fn [[sid el]]
                     (let [b (get-in el [:element/data :bcib/behaviour])
                           b' (resolve-behaviour-uuids b field-id-by-source)
                           el' (if (seq b')
                                 (assoc-in el [:element/data :bcib/behaviour] b')
                                 (update el :element/data dissoc :bcib/behaviour))]
                       [sid el']))
                   field-els))
        qset-els
        (into {}
              (map (fn [[qkw qs]]
                     (let [qid (or (source-id-from-kw qkw)
                                   (source-id-from-kw (:question-set/id qs)))
                           label (str/trim (str (:label qs)))
                           base (slug label)
                           name base
                           child-refs (->> (:fields qs)
                                           (keep (fn [fref]
                                                   (let [fid (parse-field-ref fref)]
                                                     (get-in field-els [fid :record/id]))))
                                           vec)
                           el (-> (make-sequence
                                   {:name name
                                    :label label
                                    :documentation nil
                                    :child-refs child-refs
                                    :order (or (:order qs) 0)})
                                  (assoc :record/id (u/stable-uuid (str "bcib/qset/" qid))))]
                       [qkw el]))
                   qsets))
        form-els
        (mapv (fn [[fkw form]]
                (let [fid (or (source-id-from-kw fkw)
                              (source-id-from-kw (:form/id form)))
                      label (:label form)
                      name (slug label)
                      child-refs (->> (:sections form)
                                      (keep (fn [skw]
                                              (get-in qset-els [skw :record/id])))
                                      vec)
                      el (make-sequence
                          {:name name
                           :label label
                           :documentation nil
                           :child-refs child-refs
                           :order fid})
                      el (assoc el :record/id (u/stable-uuid (str "bcib/form/" fid)))]
                  el))
              (sort-by (fn [[k _]] (or (source-id-from-kw k) 0)) forms))
        domain (emit-domain shared form-els)
        typedefs (vec (concat (vals shared) (vals @enum-cache)))
        elements (vec (concat (:elements domain)
                              form-els
                              (vals qset-els)
                              (vals field-els)))]
    {:schema (emit-schema-root legacy)
     :typedefs (vec (sort-by (comp str :record/name) typedefs))
     :elements (vec (sort-by (fn [e] [(or (:element/order e) 0)
                                      (str (:record/name e))])
                             elements))}))

(defn convert-files
  "Load legacy + overlay paths and convert."
  ([]
   (convert-files default-legacy-path default-overlay-path))
  ([legacy-path overlay-path]
   (convert (load-legacy legacy-path)
            (load-overlay overlay-path))))
