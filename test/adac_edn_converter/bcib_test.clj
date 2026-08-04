(ns adac-edn-converter.bcib-test
  "BCIB domain + survey → SchemaCraft bundle."
  (:require [adac-edn-converter.bcib.convert :as bcib]
            [clojure.test :refer [deftest is testing]]))

(defn- by-id-map
  [bundle]
  (into {} (map (juxt :record/id identity) (:elements bundle))))

(defn- by-name
  [bundle]
  (group-by :record/name (:elements bundle)))

(defn- first-named
  [by-name-map nm]
  (first (by-name-map nm)))

(defn- first-kind
  "First element with name nm and kind k."
  [by-name-map nm k]
  (first (filter #(= k (:element/kind %)) (by-name-map nm))))

(deftest convert-bcib-smoke
  (let [bundle (bcib/convert-files)
        schema (:schema bundle)
        by-name (by-name bundle)
        by-id (by-id-map bundle)
        client-list (first-named by-name :Client_list)
        client (first-kind by-name :Client :sequence)
        am-list (first-named by-name :AccountManager_list)
        am-target (first-kind by-name :AccountManager :sequence)
        am-ire (first-kind by-name :AccountManager :instance-ref)
        state-ire (first-kind by-name :State :instance-ref)
        state-target (first-kind by-name :State :sequence)
        itc (first-kind by-name :ItcPercentage :instance-ref)
        team-target (first-kind by-name :ServiceTeam :sequence)
        asset (first-kind by-name :Asset :sequence)
        asset-ire (first-kind by-name :Asset :instance-ref)
        survey-list (first-named by-name :Survey_list)
        survey (first-kind by-name :Survey :sequence)
        survey-form (first-kind by-name :SurveyForm :choice)
        bsi (first-kind by-name :BuildingSurveyInformation :sequence)
        gks (first-kind by-name :GreenkeepersShed :sequence)
        renewal (first-kind by-name :Renewal :sequence)
        rp (first-kind by-name :RenewalProgram :sequence)
        template-id (first-named by-name :TemplateId)
        scalars (filter #(= :scalar (:element/kind %)) (:elements bundle))
        ires (filter #(= :instance-ref (:element/kind %)) (:elements bundle))
        enum-tds (filter #(= :enum (:typedef/primitive %)) (:typedefs bundle))
        q383 (first-named by-name :Q383)
        q152 (first-named by-name :Q152)
        q153 (first-named by-name :Q153)
        q534 (first-named by-name :Q534)
        q457 (first-named by-name :Q457)
        select-fields
        (filter (fn [el]
                  (let [td-id (:element/typedef-id el)
                        td (first (filter #(= td-id (:record/id %))
                                          (:typedefs bundle)))]
                    (= :enum (:typedef/primitive td))))
                scalars)
        q-scalars (filter #(re-find #"^Q\d+$" (name (:record/name %))) scalars)]
    (testing "schema root"
      (is (= "BCIB" (:record/name schema)))
      (is (= bcib/schema-id (:record/id schema)))
      (is (= 1.0M (:schema/version schema))))
    (testing "kinds"
      (is (every? #(contains? #{:scalar :sequence :collection :instance-ref :choice}
                              (:element/kind %))
                  (:elements bundle)))
      (is (= 309 (count q-scalars)))
      (is (nil? q457))
      (is (= 11 (count ires))))
    (testing "top-level catalogues"
      (is (= :collection (:element/kind client-list)))
      (is (= (:record/id client) (get-in client-list [:element/data :collection/item-ref])))
      (is (some? am-list))
      (doseq [nm [:AccountManager_list :Area_list :State_list
                  :ServicePlan_list :ServiceTeam_list]]
        (is (some? (first-named by-name nm)) (str nm))))
    (testing "Client IREs"
      (is (some? client))
      (let [child-ids (set (:element/child-refs client))]
        (is (contains? child-ids (:record/id am-ire)))
        (is (contains? child-ids (:record/id state-ire)))
        (is (contains? child-ids (:record/id itc))))
      (is (= (:record/id am-target)
             (get-in am-ire [:element/instance-ref :target-id])))
      (is (= (:record/id state-target)
             (get-in state-ire [:element/instance-ref :target-id])))
      (is (= (:record/id team-target)
             (get-in itc [:element/instance-ref :target-id])))
      (is (= [(:record/id am-target)] (:element/child-refs am-ire)))
      (is (= (:element/instance-ref am-ire)
             (get-in am-ire [:element/data :instance-ref]))))
    (testing "Surveys under RenewalProgram; Asset via IRE"
      (is (some? rp))
      (is (some? survey-list))
      (is (contains? (set (:element/child-refs rp)) (:record/id survey-list)))
      (is (= (:record/id survey) (get-in survey-list [:element/data :collection/item-ref])))
      (is (some? asset))
      (is (not (contains? (set (:element/child-refs asset)) (:record/id survey-list)))
          "Asset must not nest Survey_list")
      (is (some? asset-ire))
      (is (contains? (set (:element/child-refs survey)) (:record/id asset-ire)))
      (is (= (:record/id asset)
             (get-in asset-ire [:element/instance-ref :target-id]))))
    (testing "Survey form is a choice of templates"
      (is (some? survey))
      (is (some? survey-form))
      (is (= :choice (:element/kind survey-form)))
      (is (nil? template-id) "soft TemplateId removed; choice replaces it")
      (let [survey-child-ids (set (:element/child-refs survey))
            form-alts (set (:element/child-refs survey-form))]
        (is (contains? survey-child-ids (:record/id survey-form)))
        (is (not (contains? survey-child-ids (:record/id bsi)))
            "forms must not be direct Survey children")
        (is (not (contains? survey-child-ids (:record/id gks))))
        (is (contains? form-alts (:record/id bsi)))
        (is (contains? form-alts (:record/id gks))))
      (is (= :sequence (:element/kind bsi)))
      (is (= :sequence (:element/kind gks)))
      (is (= 16 (count (:element/child-refs bsi))))
      (is (= 4 (count (:element/child-refs gks))))
      (let [bsi-names (set (map #(:record/name (by-id %)) (:element/child-refs bsi)))]
        (is (contains? bsi-names :BuildingConstruction))
        (is (contains? bsi-names :Kitchen))
        (is (contains? bsi-names :BuildingDeclaredValuesSumsInsured))))
    (testing "Renewal nests RenewalProgram"
      (is (some? renewal))
      (is (some? rp))
      (is (contains? (set (:element/child-refs renewal)) (:record/id rp))))
    (testing "shared qset UUID across forms"
      (let [bc-from-bsi (first (filter #(= :BuildingConstruction (:record/name (by-id %)))
                                       (:element/child-refs bsi)))
            bc-from-gks (first (filter #(= :BuildingConstruction (:record/name (by-id %)))
                                       (:element/child-refs gks)))]
        (is (some? bc-from-bsi))
        (is (= bc-from-bsi bc-from-gks))))
    (testing "select enums deduped"
      (is (seq enum-tds))
      (is (= 163 (count select-fields)))
      (is (< (count (set (map :element/typedef-id select-fields)))
             (count select-fields))
          "identical option lists should share typedef ids"))
    (testing "behaviour overlay and calc"
      (is (some? (get-in q383 [:element/data :bcib/behaviour :show-when :field-id])))
      (is (= 382 (get-in q383 [:element/data :bcib/behaviour :show-when :source-question-id])))
      (is (= :sum (get-in q152 [:element/data :bcib/behaviour :calc :op])))
      (is (seq (get-in q152 [:element/data :bcib/behaviour :calc :field-ids])))
      (is (every? by-id (get-in q152 [:element/data :bcib/behaviour :calc :field-ids])))
      (is (= :sum (get-in q153 [:element/data :bcib/behaviour :calc :op])))
      (is (= :sum (get-in q534 [:element/data :bcib/behaviour :calc :op])))
      (is (every? by-id (get-in q534 [:element/data :bcib/behaviour :calc :field-ids]))))
    (testing "no duplicate record ids"
      (is (= (count (:elements bundle))
             (count (set (map :record/id (:elements bundle))))))
      (is (= (count (:typedefs bundle))
             (count (set (map :record/id (:typedefs bundle)))))))))
