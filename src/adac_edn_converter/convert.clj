(ns adac-edn-converter.convert
  "Orchestrate XSD IR → SchemaCraft EDN bundle (full or Milestone-1 slice)."
  (:require [adac-edn-converter.map.elements :as el]
            [adac-edn-converter.map.typedefs :as td]
            [adac-edn-converter.util :as u]
            [adac-edn-converter.xsd.parse :as parse]))

(def schema-id
  (u/stable-uuid "schema/ADAC"))

(defn- emit-schema-root
  [schema]
  (let [doc (or (:documentation schema) "ADAC as-constructed public works schema")]
    {:record/type :schema
     :record/id schema-id
     :schema/id schema-id
     :record/parent-id nil
     :record/name "ADAC"
     :record/label "ADAC"
     :record/documentation doc
     :schema/version (try
                       (bigdec (or (:version schema) "6.0.0"))
                       (catch Exception _ 6.0M))
     :schema/status :draft
     :element/key :Schema
     :element/kind :schema
     :element/label "ADAC"
     :element/documentation doc
     :element/cardinality {:min 1 :max 1}
     :element/children []
     :element/data {}}))

(defn- slice-include?
  "Milestone-1 filter: only Sewerage → MaintenanceHoles under ProjectData."
  [slice parent-path child-name]
  (case slice
    :sewerage-mh
    (cond
      (= parent-path ["ADAC" "Project" "ProjectData"])
      (= child-name "Sewerage")
      (= parent-path ["ADAC" "Project" "ProjectData" "Sewerage"])
      (= child-name "MaintenanceHoles")
      :else true)
    true))

(defn- emit-root!
  [store *typedefs schema]
  (let [adac (get-in schema [:elements "ADAC"])
        _ (when-not adac (throw (ex-info "Missing global ADAC element" {})))
        path ["ADAC"]
        ct (:inline-complex adac)]
    ;; Keep attributes on the complex type so version is part of the first fingerprint.
    (el/emit-complex-type! store *typedefs schema schema-id
                           path "ADAC" (:documentation adac)
                           ct 0
                           :min-occurs 1 :max-occurs 1)))

(defn- seed-string-typedefs!
  [*typedefs schema]
  (doseq [n (td/string-type-names (:simple-types schema))]
    (swap! *typedefs assoc n (td/emit-typedef schema-id (:simple-types schema) n))))

(defn- seed-slice-enums!
  [*typedefs schema]
  (doseq [n ["submission_status" "asset_status" "construction_method" "data_quality"
             "sewer_mh_use" "sewer_mh_material" "sewer_mh_roofmaterial" "sewer_mh_lining"
             "sewer_mh_lidmaterial" "sewer_mh_lidtype" "sewer_mh_lidaccessrestraint"
             "sewer_mh_droptype" "sewer_mh_benching"
             "Float_Positive_Zero" "Float_Positive_NonZero" "Float_Direction"]]
    (when (and (contains? (:simple-types schema) n)
               (not (contains? @*typedefs n)))
      (swap! *typedefs assoc n (td/emit-typedef schema-id (:simple-types schema) n)))))

(defn convert
  "Convert parsed schema IR to SchemaCraft bundle.
  opts: {:slice :sewerage-mh | nil}"
  [schema & {:keys [slice]}]
  (let [*typedefs (atom {})
        store (atom (el/empty-store))]
    (seed-string-typedefs! *typedefs schema)
    (when (= slice :sewerage-mh)
      (seed-slice-enums! *typedefs schema))
    (binding [el/*include-child?* (fn [parent-path child-name]
                                    (slice-include? slice parent-path child-name))]
      (emit-root! store *typedefs schema))
    (when (nil? slice)
      (doseq [n (td/all-simple-type-names (:simple-types schema))]
        (when-not (contains? @*typedefs n)
          (swap! *typedefs assoc n (td/emit-typedef schema-id (:simple-types schema) n)))))
    {:schema (emit-schema-root schema)
     :typedefs (vec (sort-by (comp str :record/name) (vals @*typedefs)))
     :elements (vec (sort-by (fn [e] [(or (:element/order e) 0)
                                      (str (:record/name e))])
                             (el/store-elements store)))}))

(defn convert-file
  [xsd-path & opts]
  (apply convert (parse/parse-schema xsd-path) opts))

(defn convert-resource
  [resource-path & opts]
  (apply convert (parse/parse-classpath-xsd resource-path) opts))
