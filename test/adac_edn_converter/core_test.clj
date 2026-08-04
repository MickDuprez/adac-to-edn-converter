(ns adac-edn-converter.core-test
  (:require [adac-edn-converter.convert :as convert]
            [adac-edn-converter.map.elements :as el]
            [adac-edn-converter.map.typedefs :as td]
            [adac-edn-converter.util :as u]
            [adac-edn-converter.xsd.parse :as parse]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(def xsd-path
  (.getPath (io/resource "adac/ADAC_V600_Flattened.xsd")))

(def edn-readers
  {'uuid #(java.util.UUID/fromString %)})

(defn- xml-namespaced-keys
  [m]
  (filter #(and (keyword? %) (= "xml" (namespace %))) (keys m)))

(deftest stable-uuid-deterministic
  (is (= (u/stable-uuid "typedef/String_32")
         (u/stable-uuid "typedef/String_32")))
  (is (not= (u/stable-uuid "a") (u/stable-uuid "b"))))

(deftest parse-string-and-enum-types
  (let [schema (parse/parse-schema xsd-path)
        st (:simple-types schema)
        s32 (get st "String_32")
        status (get st "asset_status")]
    (is (contains? st "String_NonEmpty"))
    (is (= "String_NonEmpty" (:base s32)))
    (is (= 32 (get-in s32 [:facets :max-length])))
    (is (seq (get-in status [:facets :enumerations])))
    (is (contains? (:complex-types schema) "Feature_Sewerage_MaintenanceHole"))
    (is (contains? (:elements schema) "ADAC"))))

(deftest parse-documentation-text
  (let [schema (parse/parse-schema xsd-path)
        st (get-in schema [:simple-types "sewer_mh_lidaccessrestraint"])
        s32 (get-in schema [:simple-types "String_32"])
        gnss-doc (get-in schema [:complex-types "geometry_gnss_metadata" :documentation])]
    (is (= "Restraint applied to accessing structure" (:documentation st)))
    (is (str/includes? (:documentation s32) "32 Characters"))
    (is (not (str/includes? (str (:documentation st)) "\\s+")))
    (is (str/includes? gnss-doc "\n"))
    (is (str/includes? gnss-doc "Global Navigation Satellite System"))
    (is (str/includes? gnss-doc "Original positions may be stored"))))

(deftest normalize-doc-preserves-paragraph-newlines
  (is (= "First paragraph\nSecond paragraph"
         (el/normalize-doc "  First   paragraph  \n\n  Second    paragraph  ")))
  (is (= "Single line" (el/normalize-doc "  Single   line  "))))

(deftest typedef-string-and-enum-emission
  (let [schema (parse/parse-schema xsd-path)
        st (:simple-types schema)
        sid (u/stable-uuid "schema/test")
        string-td (td/emit-typedef sid st "String_32")
        enum-td (td/emit-typedef sid st "asset_status")]
    (is (= :typedef (:element/kind string-td)))
    (is (= :string (:typedef/primitive string-td)))
    (is (= 32 (get-in string-td [:typedef/facets :max-length])))
    (is (true? (get-in string-td [:typedef/facets :trim?])))
    (is (= "String 32" (:record/label string-td)))
    (is (= :enum (:typedef/primitive enum-td)))
    (is (vector? (get-in enum-td [:typedef/facets :items])))
    (is (empty? (xml-namespaced-keys string-td)))))

(deftest labels-use-humanized-name-not-documentation
  (let [schema (parse/parse-schema xsd-path)
        st (:simple-types schema)
        sid (u/stable-uuid "schema/test")
        td (td/emit-typedef sid st "sewer_mh_lidaccessrestraint")]
    (is (= "Sewer Mh Lidaccessrestraint" (:record/label td)))
    (is (= "Sewer Mh Lidaccessrestraint" (:element/label td)))
    (is (= :sewer_mh_lidaccessrestraint (:record/name td)))
    (is (= "Restraint applied to accessing structure" (:record/documentation td)))))

(deftest element-label-humanize
  (is (= "Sewer Mh Lidaccessrestraint" (u/element-label "sewer_mh_lidaccessrestraint")))
  (is (= "String 32" (u/element-label "String_32")))
  (is (= "MaintenanceHoles" (u/element-label "MaintenanceHoles")))
  (is (= "CI Concrete" (u/element-label "CI_Concrete"))))

(deftest collection-wrapper-detection
  (is (el/collection-wrapper?
       {:kind :sequence
        :particles [{:kind :element :name "MaintenanceHole" :max-occurs :many :min-occurs 0}]}))
  (is (el/collection-wrapper?
       {:kind :sequence
        :particles [{:kind :element :name "Vertex" :max-occurs 2 :min-occurs 2}]})
      "finite maxOccurs > 1 is a collection wrapper")
  (is (not (el/collection-wrapper?
            {:kind :sequence
             :particles [{:kind :element :name "A" :max-occurs 1}
                         {:kind :element :name "B" :max-occurs 1}]}))))

(deftest collection-cardinality-from-item-not-wrapper
  "Required plural wrappers may be empty; list :min comes from the item particle."
  (let [store (atom (el/empty-store))
        *typedefs (atom {})
        sid (u/stable-uuid "schema/coll-card-test")
        schema {:simple-types {"String_254" {:base "xs:string"
                                             :facets {:max-length 254}}}
                :complex-types {}}
        ;; SupportingFiles-style: wrapper min=1 nillable, child min=0 unbounded
        empty-allowed
        (el/emit-complex-type!
         store *typedefs schema sid ["Root"] "SupportingFiles" nil
         {:kind :complex
          :particles [{:kind :sequence :min-occurs 1 :max-occurs 1
                       :particles [{:kind :element :name "SupportingFile"
                                    :min-occurs 0 :max-occurs :many
                                    :nillable? false
                                    :type-ref "String_254"}]}]}
         0
         :min-occurs 1 :max-occurs 1 :nillable? true)
        ;; MaintenanceHoles-style: wrapper min=1 not nillable, child min=0
        mh-style
        (el/emit-complex-type!
         store *typedefs schema sid ["Root"] "Things" nil
         {:kind :complex
          :particles [{:kind :sequence :min-occurs 1 :max-occurs 1
                       :particles [{:kind :element :name "Thing"
                                    :min-occurs 0 :max-occurs :many
                                    :nillable? false
                                    :type-ref "String_254"}]}]}
         1
         :min-occurs 1 :max-occurs 1 :nillable? false)
        ;; True required list: child min=1 unbounded
        required-list
        (el/emit-complex-type!
         store *typedefs schema sid ["Root"] "Points" nil
         {:kind :complex
          :particles [{:kind :sequence :min-occurs 1 :max-occurs 1
                       :particles [{:kind :element :name "Point"
                                    :min-occurs 1 :max-occurs :many
                                    :nillable? false
                                    :type-ref "String_254"}]}]}
         2
         :min-occurs 1 :max-occurs 1)
        ;; Geometry min≥2 + finite max
        vertices
        (el/emit-complex-type!
         store *typedefs schema sid ["Root"] "Segment" nil
         {:kind :complex
          :particles [{:kind :sequence :min-occurs 1 :max-occurs 1
                       :particles [{:kind :element :name "Vertex"
                                    :min-occurs 2 :max-occurs 2
                                    :nillable? false
                                    :type-ref "String_254"}]}]}
         3
         :min-occurs 1 :max-occurs 1)
        by-id (into {} (map (juxt :record/id identity) (el/store-elements store)))
        supporting-file (by-id (get-in empty-allowed [:element/data :collection/item-ref]))]
    (is (= :collection (:element/kind empty-allowed)))
    (is (= {:min 0 :max :many} (:element/cardinality empty-allowed)))
    (is (true? (:element/nillable? empty-allowed)))
    (is (= {:min 0 :max :many} (:element/cardinality mh-style)))
    (is (nil? (:element/nillable? mh-style)))
    (is (= {:min 1 :max :many} (:element/cardinality required-list)))
    (is (= {:min 2 :max 2} (:element/cardinality vertices)))
    (is (= {:min 1 :max 1} (:element/cardinality supporting-file))
        "collection item Element is one instance, not 0..unbounded")))

(deftest unbounded-choice-collection-min-from-choice
  "Choice maxOccurs=unbounded minOccurs=0 → empty collection allowed."
  (let [store (atom (el/empty-store))
        *typedefs (atom {})
        sid (u/stable-uuid "schema/choice-coll-test")
        schema {:simple-types {"String_32" {:base "xs:string"
                                            :facets {:max-length 32}}}
                :complex-types {}}
        pathways
        (el/emit-complex-type!
         store *typedefs schema sid ["Root"] "Pathways" nil
         {:kind :complex
          :particles [{:kind :choice :min-occurs 0 :max-occurs :many
                       :particles [{:kind :element :name "Path"
                                    :min-occurs 1 :max-occurs 1
                                    :type-ref "String_32"}
                                   {:kind :element :name "CycleWay"
                                    :min-occurs 1 :max-occurs 1
                                    :type-ref "String_32"}]}]}
         0
         :min-occurs 1 :max-occurs 1)]
    (is (= :collection (:element/kind pathways)))
    (is (= {:min 0 :max :many} (:element/cardinality pathways)))))

(deftest example-schema-contract-smoke
  (let [path "doc/adac-importer-pack/fixtures/example-schema.edn"
        bundle (edn/read-string {:readers edn-readers} (slurp path))]
    (is (map? (:schema bundle)))
    (is (= :schema (get-in bundle [:schema :element/kind])))
    (is (vector? (:typedefs bundle)))
    (is (vector? (:elements bundle)))
    (is (some #(= :typedef (:element/kind %)) (:typedefs bundle)))
    (is (some #(= :collection (:element/kind %)) (:elements bundle)))
    (is (some #(= :scalar (:element/kind %)) (:elements bundle)))
    (is (every? empty? (map xml-namespaced-keys
                            (concat [(:schema bundle)]
                                    (:typedefs bundle)
                                    (:elements bundle)))))))

(deftest milestone1-sewerage-mh-slice
  (let [expectations (edn/read-string (slurp "test/fixtures/slice-sewerage-mh.edn"))
        bundle (convert/convert-file xsd-path :slice :sewerage-mh)
        names (set (map :record/name (:elements bundle)))
        tnames (set (map :record/name (:typedefs bundle)))
        mh-coll (some #(when (and (= :MaintenanceHoles (:record/name %))
                                  (= :collection (:element/kind %)))
                         %)
                      (:elements bundle))
        sf-coll (some #(when (and (= :SupportingFiles (:record/name %))
                                  (= :collection (:element/kind %)))
                         %)
                      (:elements bundle))
        mh-item (when mh-coll
                  (some #(when (= (:record/id %)
                                  (get-in mh-coll [:element/data :collection/item-ref]))
                           %)
                        (:elements bundle)))
        infra (some #(when (= :InfrastructureCode (:record/name %)) %)
                    (:elements bundle))
        version (some #(when (= :version (:record/name %)) %)
                      (:elements bundle))]
    (is (= "ADAC" (get-in bundle [:schema :record/name])))
    (doseq [n (:required-typedef-names expectations)]
      (is (contains? tnames n) (str "missing typedef " n)))
    (doseq [n (:required-element-names expectations)]
      (is (contains? names n) (str "missing element " n)))
    (doseq [n (:excluded-element-names expectations)]
      (is (not (contains? names n)) (str "should exclude " n)))
    (is (some? mh-coll))
    (is (= {:min 0 :max :many} (:element/cardinality mh-coll))
        "MaintenanceHoles may be empty")
    (is (uuid? (get-in mh-coll [:element/data :collection/item-ref])))
    (is (= {:min 1 :max 1} (:element/cardinality mh-item))
        "MaintenanceHole item is one instance")
    (is (some? sf-coll))
    (is (= {:min 0 :max :many} (:element/cardinality sf-coll))
        "SupportingFiles may be empty")
    (is (true? (:element/nillable? sf-coll)))
    (is (true? (:element/nillable? infra)))
    (is (= :parent-property (:element/placement version)))
    (is (= "version" (:element/property-name version)))
    (is (= "6.0.0" (:element/fixed version)))
    (is (= "6.0.0" (get-in version [:element/data :fixed])))
    (is (every? empty? (map xml-namespaced-keys
                            (concat [(:schema bundle)]
                                    (:typedefs bundle)
                                    (:elements bundle)))))
    (is (every? #(= :typedef (:element/kind %)) (:typedefs bundle)))
    (is (every? #(contains? #{:scalar :sequence :collection :choice :host-event}
                            (:element/kind %))
                (:elements bundle)))))

(deftest geometry-is-host-event-with-point-target
  (let [bundle (convert/convert-file xsd-path :slice :sewerage-mh)
        by-id (into {} (map (juxt :record/id identity) (:elements bundle)))
        by-name (group-by :record/name (:elements bundle))
        geometry (first (by-name :Geometry))
        target-id (first (:element/child-refs geometry))
        point (by-id target-id)
        host-action (:element/host-action geometry)
        ordinary-names #{:Point :X :Y :Z :GNSSMetadata}]
    (is (some? geometry))
    (is (= :host-event (:element/kind geometry)))
    (is (= :host-event (:element/type geometry)))
    (is (= 1 (count (:element/child-refs geometry))))
    (is (= :Point (:record/name point)))
    (is (= :sequence (:element/kind point)))
    (is (= target-id (:target-id host-action)))
    (is (= target-id (get-in geometry [:element/data :host-action :target-id])))
    (is (= (:element/child-refs geometry)
           (get-in geometry [:element/data :child-refs])))
    (is (= "Geometry" (:heading host-action)))
    (is (= "Capture geometry from CAD" (:label host-action)))
    (is (= "onCaptureGeometry" (:handler host-action)))
    (doseq [n ordinary-names]
      (is (every? #(not= :host-event (:element/kind %)) (by-name n))
          (str n " must remain an ordinary Element")))
    (is (= 1 (count (by-name :Point)))
        "equal geometry Targets reuse one UUID")))

(deftest shared-elements-are-deduped
  (let [bundle (convert/convert-file xsd-path :slice :sewerage-mh)
        by-name (group-by :record/name (:elements bundle))
        engineer (first (by-name :Engineer))
        surveyor (first (by-name :Surveyor))
        date-approved (by-name :DateApproved)
        parents-with-engineer
        (filter #(some #{(:record/id engineer)} (:element/child-refs %))
                (:elements bundle))]
    (is (= 1 (count (by-name :Engineer))))
    (is (= 1 (count (by-name :Surveyor))))
    (is (= 1 (count (by-name :GNSSMetadata)))
        "GNSSMetadata should be a single shared sequence when shapes match")
    ;; Surveyor vs Engineer DateApproved texts differ → two shapes (correct)
    (is (= 2 (count date-approved)))
    (is (= 2 (count (set (map :record/documentation date-approved)))))
    (is (>= (count parents-with-engineer) 2)
        "Engineer id reused by multiple parents")
    (is (every? #(some #{(:record/id engineer)} (:element/child-refs %))
                parents-with-engineer))
    (is (some #(some #{(:record/id surveyor)} (:element/child-refs %))
              (:elements bundle)))))

(deftest geometry-host-event-detection
  (is (true? (el/geometry-host-event? "Geometry" "geometry_point_singlepoint")))
  (is (false? (el/geometry-host-event? "Point" "geometry_position_3d")))
  (is (false? (el/geometry-host-event? "Geometry" "String_32"))))

(deftest host-event-registration-and-target-reuse
  (let [store (atom (el/empty-store))
        sid (u/stable-uuid "schema/host-event-test")
        target-a (el/register-complex!
                  store
                  {:schema-id sid :name "Point" :documentation "pt"
                   :kind :sequence :min-occurs 1 :max-occurs 1
                   :child-refs [] :order 0})
        target-b (el/register-complex!
                  store
                  {:schema-id sid :name "Point" :documentation "pt"
                   :kind :sequence :min-occurs 1 :max-occurs 1
                   :child-refs [] :order 1})
        g1 (el/register-host-event!
            store
            {:schema-id sid :name "Geometry"
             :documentation "The geometry representing this feature."
             :min-occurs 1 :max-occurs 1
             :target-id (:record/id target-a) :order 0})
        g2 (el/register-host-event!
            store
            {:schema-id sid :name "Geometry"
             :documentation "The geometry representing this feature."
             :min-occurs 1 :max-occurs 1
             :target-id (:record/id target-b) :order 2})]
    (is (= (:record/id target-a) (:record/id target-b))
        "equal Targets reuse one UUID")
    (is (= (:record/id g1) (:record/id g2))
        "equal Geometry host-events reuse one UUID")
    (is (= :host-event (:element/kind g1)))
    (is (= (:record/id target-a)
           (get-in g1 [:element/host-action :target-id])))))

(deftest find-or-create-respects-shape-differences
  (let [store (atom (el/empty-store))
        sid (u/stable-uuid "schema/test")
        typedef-a (u/stable-uuid "typedef/a")
        typedef-b (u/stable-uuid "typedef/b")
        a1 (el/register-scalar! store {:schema-id sid :name "X" :documentation "coord"
                                       :nillable? false :min-occurs 1 :max-occurs 1
                                       :typedef-id typedef-a :order 0})
        a2 (el/register-scalar! store {:schema-id sid :name "X" :documentation "coord"
                                       :nillable? false :min-occurs 1 :max-occurs 1
                                       :typedef-id typedef-a :order 5})
        b (el/register-scalar! store {:schema-id sid :name "X" :documentation "coord"
                                      :nillable? true :min-occurs 1 :max-occurs 1
                                      :typedef-id typedef-a :order 0})
        c (el/register-scalar! store {:schema-id sid :name "X" :documentation "other"
                                      :nillable? false :min-occurs 1 :max-occurs 1
                                      :typedef-id typedef-b :order 0})
        xs (filter #(= :X (:record/name %)) (el/store-elements store))]
    (is (= (:record/id a1) (:record/id a2)))
    (is (= 3 (count xs)))
    (is (not= (:record/id a1) (:record/id b)))
    (is (not= (:record/id a1) (:record/id c)))))

(deftest fixed-scalar-shape-and-emission
  (let [schema (parse/parse-schema xsd-path)
        adac (get-in schema [:elements "ADAC"])
        version-attr (some #(when (= "version" (:name %)) %)
                           (or (get-in adac [:inline-complex :attributes]) []))
        store (atom (el/empty-store))
        sid (u/stable-uuid "schema/fixed-test")
        typedef-id (u/stable-uuid "typedef/string")
        s1 (el/register-scalar!
            store
            {:schema-id sid :name "Structure" :documentation "disc"
             :nillable? false :min-occurs 1 :max-occurs 1
             :typedef-id typedef-id :order 0 :fixed "In Ground"})
        s2 (el/register-scalar!
            store
            {:schema-id sid :name "Structure" :documentation "disc"
             :nillable? false :min-occurs 1 :max-occurs 1
             :typedef-id typedef-id :order 1 :fixed "In Ground"})
        s3 (el/register-scalar!
            store
            {:schema-id sid :name "Structure" :documentation "disc"
             :nillable? false :min-occurs 1 :max-occurs 1
             :typedef-id typedef-id :order 2 :fixed "CycleWay"})]
    (is (= "6.0.0" (:fixed version-attr)))
    (is (= (:record/id s1) (:record/id s2))
        "identical fixed scalars reuse one UUID")
    (is (not= (:record/id s1) (:record/id s3))
        "different fixed values yield distinct Elements")
    (is (= "In Ground" (:element/fixed s1)))
    (is (= "In Ground" (get-in s1 [:element/data :fixed])))))
