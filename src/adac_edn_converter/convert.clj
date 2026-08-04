(ns adac-edn-converter.convert
  "Orchestrate XSD IR → SchemaCraft EDN bundle (ADAC or LandXML profiles)."
  (:require [adac-edn-converter.map.elements :as el]
            [adac-edn-converter.map.typedefs :as td]
            [adac-edn-converter.util :as u]
            [adac-edn-converter.xsd.parse :as parse]))

(def adac-schema-id
  (u/stable-uuid "schema/ADAC"))

(def schema-id
  "Backward-compatible alias for ADAC schema id."
  adac-schema-id)

(def landxml-schema-id
  (u/stable-uuid "schema/LandXML-1.2"))

(defn- emit-schema-root
  [schema {:keys [schema-id name label version-default]}]
  (let [doc (or (:documentation schema)
                (str name " schema"))]
    {:record/type :schema
     :record/id schema-id
     :schema/id schema-id
     :record/parent-id nil
     :record/name name
     :record/label label
     :record/documentation doc
     :schema/version (try
                       (bigdec (or (:version schema) version-default))
                       (catch Exception _
                         (try (bigdec version-default)
                              (catch Exception _ 1.0M))))
     :schema/status :draft
     :element/key :Schema
     :element/kind :schema
     :element/label label
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
  [store *typedefs schema schema-id root-name]
  (let [root-el (get-in schema [:elements root-name])
        _ (when-not root-el
            (throw (ex-info (str "Missing global " root-name " element")
                            {:root root-name})))
        path [root-name]
        ct (:inline-complex root-el)]
    (when-not ct
      (throw (ex-info "Root element missing inline complexType"
                      {:root root-name})))
    (el/emit-complex-type! store *typedefs schema schema-id
                           path root-name (:documentation root-el)
                           ct 0
                           :min-occurs 1 :max-occurs 1)))

(defn- seed-string-typedefs!
  [*typedefs schema schema-id]
  (doseq [n (td/string-type-names (:simple-types schema))]
    (swap! *typedefs assoc n (td/emit-typedef schema-id (:simple-types schema) n))))

(defn- seed-slice-enums!
  [*typedefs schema schema-id]
  (doseq [n ["submission_status" "asset_status" "construction_method" "data_quality"
             "sewer_mh_use" "sewer_mh_material" "sewer_mh_roofmaterial" "sewer_mh_lining"
             "sewer_mh_lidmaterial" "sewer_mh_lidtype" "sewer_mh_lidaccessrestraint"
             "sewer_mh_droptype" "sewer_mh_benching"
             "Float_Positive_Zero" "Float_Positive_NonZero" "Float_Direction"]]
    (when (and (contains? (:simple-types schema) n)
               (not (contains? @*typedefs n)))
      (swap! *typedefs assoc n (td/emit-typedef schema-id (:simple-types schema) n)))))

(defn- seed-all-simple-typedefs!
  [*typedefs schema schema-id]
  (doseq [n (td/all-simple-type-names (:simple-types schema))]
    (when-not (contains? @*typedefs n)
      (swap! *typedefs assoc n (td/emit-typedef schema-id (:simple-types schema) n)))))

(defn- profile-config
  [profile]
  (case profile
    :landxml
    {:schema-id landxml-schema-id
     :root-name "LandXML"
     :schema-name "LandXML"
     :schema-label "LandXML"
     :version-default "1.2"
     :seed-all-simple? true
     :host-events? false
     :segment-host-events? true
     :flatten-type-choices? true}
    ;; default ADAC
    {:schema-id adac-schema-id
     :root-name "ADAC"
     :schema-name "ADAC"
     :schema-label "ADAC"
     :version-default "6.0.0"
     :seed-all-simple? false
     :host-events? true}))

(defn convert
  "Convert parsed schema IR to SchemaCraft bundle.
  opts: {:slice :sewerage-mh | nil
         :profile :adac | :landxml}"
  [schema & {:keys [slice profile] :or {profile :adac}}]
  (let [cfg (profile-config profile)
        schema-id (:schema-id cfg)
        *typedefs (atom {})
        store (atom (el/empty-store))]
    (seed-string-typedefs! *typedefs schema schema-id)
    (when (and (= profile :adac) (= slice :sewerage-mh))
      (seed-slice-enums! *typedefs schema schema-id))
    (when (or (:seed-all-simple? cfg) (nil? slice))
      (seed-all-simple-typedefs! *typedefs schema schema-id))
    (binding [el/*include-child?* (when (= profile :adac)
                                    (fn [parent-path child-name]
                                      (slice-include? slice parent-path child-name)))
              el/*geometry-host-events?* (boolean (:host-events? cfg))
              el/*segment-host-events?* (boolean (:segment-host-events? cfg))
              el/*flatten-type-choices?* (boolean (:flatten-type-choices? cfg))]
      (emit-root! store *typedefs schema schema-id (:root-name cfg)))
    {:schema (emit-schema-root schema
                               {:schema-id schema-id
                                :name (:schema-name cfg)
                                :label (:schema-label cfg)
                                :version-default (:version-default cfg)})
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
