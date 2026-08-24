(ns adac-edn-converter.landxml.survey
  "LandXML instance XML → ADAC Geometry survey overlay catalog.

  Extracts CgPoints plus PlanFeature / Chain / Alignment / Parcel linework.
  Does not route through the LandXML SchemaCraft schema bundle."
  (:require [adac-edn-converter.instance.xml-util :as xu]
            [adac-edn-converter.landxml.coords :as coords]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def ^:private linework-parents
  #{"PlanFeature" "Alignment" "Parcel"})

(def ^:private geom-segment-names
  #{"Line" "Curve" "Spiral" "IrregularLine" "Chain"})

(defn- local
  [node]
  (xu/tag-local (:tag node)))

(defn- eager-element
  "Realize a possibly-lazy data.xml element tree so the source stream can close."
  [node]
  (cond
    (xu/element? node)
    (-> node
        (update :attrs #(into {} %))
        (update :content (fn [c]
                           (mapv eager-element (or c [])))))
    (string? node) node
    :else node))

(defn- element-seq
  "Depth-first seq of element nodes."
  [node]
  (when (xu/element? node)
    (cons node (mapcat element-seq (xu/element-children node)))))

(defn- attr
  [node k]
  (let [v (not-empty (str/trim (str (or (get (xu/attrs-by-local node) k) ""))))]
    v))

(defn- identity-fields
  [node]
  (cond-> {}
    (attr node "name") (assoc :name (attr node "name"))
    (attr node "class") (assoc :class (attr node "class"))
    (attr node "code") (assoc :code (attr node "code"))
    (attr node "desc") (assoc :desc (attr node "desc"))
    (attr node "oID") (assoc :oID (attr node "oID"))
    (attr node "state") (assoc :state (attr node "state"))))

(defn- feature-properties
  "Flatten LandXML Feature/Property children into {label value}."
  [node]
  (let [props (mapcat (fn [feat]
                        (for [p (get (xu/children-by-name feat) "Property")
                              :let [a (xu/attrs-by-local p)
                                    label (not-empty (str/trim (str (or (get a "label") ""))))
                                    value (get a "value")]
                              :when label]
                          [label (str value)]))
                      (get (xu/children-by-name node) "Feature"))]
    (when (seq props)
      (into {} props))))

(defn- compact
  [m]
  (into {}
        (remove (fn [[_ v]]
                  (or (nil? v)
                      (and (coll? v) (empty? v) (not (record? v)))))
                m)))

(defn- parse-cg-point
  [node]
  (let [name (attr node "name")
        pos (coords/parse-point-text (xu/text-content node))]
    (when name
      {:name name
       :position pos
       :fields (identity-fields node)
       :properties (feature-properties node)
       :node node})))

(defn- index-cg-points
  "All CgPoint elements → {:index {name position} :points [parsed…]}"
  [root]
  (let [parsed (->> (element-seq root)
                    (filter #(= "CgPoint" (local %)))
                    (keep parse-cg-point))]
    {:index (into {}
                  (keep (fn [p]
                          (when (:position p)
                            [(:name p) (:position p)])))
                  parsed)
     :points parsed}))

(defn- coord-system-map
  [node]
  (when node
    (compact
     {:datum (attr node "datum")
      :horizontal-datum (attr node "horizontalDatum")
      :vertical-datum (attr node "verticalDatum")
      :desc (attr node "desc")
      :epsg-code (attr node "epsgCode")
      :name (attr node "name")})))

(defn- read-coordinate-system
  [root]
  (or (coord-system-map (coords/child-named root "CoordinateSystem"))
      (coord-system-map
       (first (filter #(= "CoordinateSystem" (local %)) (element-seq root))))))

(defn- warn!
  [warnings msg]
  (swap! warnings conj msg))

(defn- context-label
  [fields source]
  (or (:name fields)
      (:oID fields)
      (name source)))

(defn- resolve-point
  [node point-index warnings fields source pnt-refs]
  (let [r (coords/point-from-node node point-index)]
    (when (:missing-ref r)
      (warn! warnings
             (str "Unresolved pntRef \"" (:missing-ref r) "\" on "
                  (context-label fields source)))
      (swap! pnt-refs conj (:missing-ref r)))
    (when (:pnt-ref r)
      (swap! pnt-refs conj (:pnt-ref r)))
    (:position r)))

(defn- first-child
  [node local-name]
  (coords/child-named node local-name))

(defn- chain-names
  [node]
  (->> (str/split (str/trim (xu/text-content node)) #"\s+")
       (remove str/blank?)
       vec))

(defn- resolve-named-points
  [names point-index warnings fields source pnt-refs]
  (into []
        (keep (fn [n]
                (if-let [p (get point-index n)]
                  (do (swap! pnt-refs conj n) p)
                  (do (warn! warnings
                             (str "Unresolved pntRef \"" n "\" on "
                                  (context-label fields source)))
                      (swap! pnt-refs conj n)
                      nil))))
        names))

(defn- irregular-vertices
  [node point-index warnings fields source pnt-refs]
  (let [list3 (first-child node "PntList3D")
        list2 (first-child node "PntList2D")
        from-list (cond
                    list3 (coords/parse-pnt-list (xu/text-content list3) 3)
                    list2 (coords/parse-pnt-list (xu/text-content list2) 2)
                    :else nil)
        start (resolve-point (first-child node "Start") point-index warnings fields source pnt-refs)
        end (resolve-point (first-child node "End") point-index warnings fields source pnt-refs)]
    (if (seq from-list)
      from-list
      (into [] (remove nil? [start end])))))

(defn- flush-poly
  [acc current]
  (if (>= (count current) 2)
    (conj acc {:PolySegment (vec current)})
    acc))

(defn- append-vertices
  "Join a run of vertices onto `current`, avoiding a duplicate join vertex."
  [current verts]
  (cond
    (empty? verts) current
    (empty? current) (vec verts)
    (coords/same-point? (last current) (first verts))
    (into (vec current) (rest verts))
    :else
    :disconnect))

(defn- curve-clockwise?
  [node]
  (let [rot (str/lower-case (str (or (attr node "rot") "")))]
    (contains? #{"cw" "clockwise"} rot)))

(defn- segments-of
  "CoordGeom children that are GeomList segments, document order."
  [coord-node]
  (filter #(contains? geom-segment-names (local %))
          (xu/element-children coord-node)))

(defn- coord-geoms
  [parent]
  (get (xu/children-by-name parent) "CoordGeom" []))

(defn- convert-segments
  "GeomList segments → {:fragments […] :spiral? bool} using `point-index`."
  [segment-nodes point-index warnings fields source pnt-refs]
  (loop [nodes segment-nodes
         acc []
         current []]
    (if (empty? nodes)
      {:fragments (flush-poly acc current)}
      (let [node (first nodes)
            tag (local node)]
        (case tag
          "Line"
          (let [s (resolve-point (first-child node "Start") point-index warnings fields source pnt-refs)
                e (resolve-point (first-child node "End") point-index warnings fields source pnt-refs)]
            (if (and s e)
              (let [joined (append-vertices current [s e])]
                (if (= :disconnect joined)
                  (recur (rest nodes) (flush-poly acc current) [s e])
                  (recur (rest nodes) acc joined)))
              (recur (rest nodes) acc current)))

          "Chain"
          (let [verts (resolve-named-points (chain-names node) point-index
                                            warnings fields source pnt-refs)]
            (if (>= (count verts) 2)
              (let [joined (append-vertices current verts)]
                (if (= :disconnect joined)
                  (recur (rest nodes) (flush-poly acc current) verts)
                  (recur (rest nodes) acc joined)))
              (recur (rest nodes) acc current)))

          "IrregularLine"
          (let [verts (irregular-vertices node point-index warnings fields source pnt-refs)]
            (if (>= (count verts) 2)
              (let [joined (append-vertices current verts)]
                (if (= :disconnect joined)
                  (recur (rest nodes) (flush-poly acc current) verts)
                  (recur (rest nodes) acc joined)))
              (recur (rest nodes) acc current)))

          "Curve"
          (let [from (resolve-point (first-child node "Start") point-index warnings fields source pnt-refs)
                to (resolve-point (first-child node "End") point-index warnings fields source pnt-refs)
                centre (resolve-point (first-child node "Center") point-index warnings fields source pnt-refs)
                flushed (flush-poly acc current)]
            (if (and from to centre)
              (recur (rest nodes)
                     (conj flushed {:CurveCircular {:FromPoint from
                                                    :ToPoint to
                                                    :CentrePoint centre
                                                    :Clockwise (curve-clockwise? node)}})
                     [])
              (recur (rest nodes) flushed [])))

          "Spiral"
          (do
            (warn! warnings
                   (str "Spiral skipped on " (context-label fields source)
                        " (no ADAC type)"))
            (recur (rest nodes) (flush-poly acc current) []))

          (recur (rest nodes) acc current))))))

(defn- fragment-start
  [f]
  (cond
    (:PolySegment f) (first (:PolySegment f))
    (:CurveCircular f) (get-in f [:CurveCircular :FromPoint])
    :else nil))

(defn- fragment-end
  [f]
  (cond
    (:PolySegment f) (last (:PolySegment f))
    (:CurveCircular f) (get-in f [:CurveCircular :ToPoint])
    :else nil))

(defn- drop-closing-vertex
  "If a single PolySegment repeats its first vertex at the end, drop the duplicate."
  [fragments]
  (if (and (= 1 (count fragments))
           (:PolySegment (first fragments)))
    (let [vs (:PolySegment (first fragments))]
      (if (and (>= (count vs) 4)
               (coords/same-point? (first vs) (last vs)))
        [{:PolySegment (vec (butlast vs))}]
        fragments))
    fragments))

(defn- unique-ring-vertex-count
  [fragments]
  (let [verts (when (and (= 1 (count fragments))
                         (:PolySegment (first fragments)))
                (:PolySegment (first fragments)))]
    (cond
      (and (seq verts) (coords/same-point? (first verts) (last verts)))
      (dec (count verts))
      (seq verts) (count verts)
      :else (count fragments))))

(defn- closed-ring?
  [fragments]
  (let [s (fragment-start (first fragments))
        e (fragment-end (last fragments))
        mixed? (not (and (= 1 (count fragments))
                         (:PolySegment (first fragments))))]
    (and (seq fragments)
         s e
         (coords/same-point? s e)
         (or mixed? (>= (unique-ring-vertex-count fragments) 3)))))

(defn- path-body
  [fragments]
  (if (and (= 1 (count fragments))
           (contains? (first fragments) :PolySegment))
    (first fragments)
    (vec fragments)))

(defn- geometry-for-fragments
  [fragments]
  (when (seq fragments)
    (if (closed-ring? fragments)
      (let [fr (drop-closing-vertex fragments)
            body (path-body fr)
            verts (when (map? body) (:PolySegment body))]
        (if (and verts (< (count verts) 3))
          {:kind :polyline :Geometry {:Polyline {:Path body}}}
          {:kind :polygon :Geometry {:Polygon {:Ring body}}}))
      {:kind :polyline :Geometry {:Polyline {:Path (path-body fragments)}}})))

(defn- linework-geometry
  [parent point-index warnings source]
  (let [fields (identity-fields parent)
        pnt-refs (atom [])
        cgs (coord-geoms parent)
        segs (mapcat segments-of cgs)
        {:keys [fragments]} (convert-segments segs point-index warnings fields source pnt-refs)
        geom (geometry-for-fragments fragments)
        base {:fields fields
              :properties (feature-properties parent)
              :pnt-refs (vec @pnt-refs)
              :source source}]
    (cond
      geom (merge base geom)
      (seq cgs) base
      :else nil)))

(defn- locations-geometry
  [parent point-index warnings source]
  (let [fields (identity-fields parent)
        pnt-refs (atom [])
        locs (get (xu/children-by-name parent) "Location" [])
        pts (keep #(resolve-point % point-index warnings fields source pnt-refs) locs)]
    (when (seq pts)
      (cond-> {:fields fields
               :properties (feature-properties parent)
               :pnt-refs (vec @pnt-refs)
               :source source}
        (= 1 (count pts))
        (assoc :kind :point :Geometry {:Point (first pts)})
        (> (count pts) 1)
        (assoc :kind :multipoint :Geometry {:MultiPoint {:Point (vec pts)}})))))

(defn- plan-feature-geometry
  [node point-index warnings]
  (or (linework-geometry node point-index warnings :plan-feature)
      (locations-geometry node point-index warnings :plan-feature)
      {:source :plan-feature
       :fields (identity-fields node)
       :properties (feature-properties node)}))

(defn- next-id
  [used candidate fallback]
  (let [base (or (not-empty candidate) fallback)]
    (loop [id base n 2]
      (if (contains? @used id)
        (recur (str base "#" n) (inc n))
        (do (swap! used conj id) id)))))

(defn- emit-object
  [used {:keys [kind source fields properties pnt-refs Geometry]} fallback]
  (let [id (next-id used (or (:name fields) (:oID fields)) fallback)]
    (compact
     (merge {:id id :source source}
            (when kind {:kind kind})
            (when Geometry {:Geometry Geometry})
            (select-keys fields [:name :class :code :desc :oID :state])
            (when (seq properties) {:properties properties})
            (when (seq pnt-refs) {:pnt-refs pnt-refs})))))

(defn- ancestor-linework?
  "True if `node` has a PlanFeature/Alignment/Parcel ancestor (exclusive)."
  [node parent-of]
  (loop [p (get parent-of node)]
    (cond
      (nil? p) false
      (contains? linework-parents (local p)) true
      :else (recur (get parent-of p)))))

(defn- parent-index
  [root]
  (reduce (fn [m n]
            (reduce #(assoc %1 %2 n) m (xu/element-children n)))
          {}
          (element-seq root)))

(defn- fallback-id
  [source idx]
  (str (name source) "-" (inc idx)))

(defn- collect-linework
  [root point-index warnings used]
  (let [parent-of (parent-index root)
        nodes (element-seq root)
        plan-features (filter #(= "PlanFeature" (local %)) nodes)
        alignments (filter #(= "Alignment" (local %)) nodes)
        parcels (filter #(= "Parcel" (local %)) nodes)
        chains (filter #(= "Chain" (local %)) nodes)
        pf-objs (keep-indexed
                 (fn [i n]
                   (when-let [g (plan-feature-geometry n point-index warnings)]
                     (emit-object used g (fallback-id :plan-feature i))))
                 plan-features)
        al-objs (keep-indexed
                 (fn [i n]
                   (when-let [g (linework-geometry n point-index warnings :alignment)]
                     (emit-object used g (fallback-id :alignment i))))
                 alignments)
        pc-objs (keep-indexed
                 (fn [i n]
                   (when-let [g (linework-geometry n point-index warnings :parcel)]
                     (emit-object used g (fallback-id :parcel i))))
                 parcels)
        ch-objs (keep-indexed
                 (fn [i n]
                   (when-not (ancestor-linework? n parent-of)
                     (let [fields (identity-fields n)
                           pnt-refs (atom [])
                           {:keys [fragments]} (convert-segments [n] point-index warnings
                                                                 fields :chain pnt-refs)
                           geom (geometry-for-fragments fragments)]
                       (when geom
                         (emit-object used
                                      (assoc geom
                                             :fields fields
                                             :properties (feature-properties n)
                                             :pnt-refs (vec @pnt-refs)
                                             :source :chain)
                                      (fallback-id :chain i))))))
                 chains)]
    (into [] (concat pf-objs al-objs pc-objs ch-objs))))

(defn- emit-cg-points
  [parsed used]
  (vec
   (keep-indexed
    (fn [i p]
      (when-let [pos (:position p)]
        (emit-object used
                     {:kind :point
                      :source :cg-point
                      :fields (:fields p)
                      :properties (:properties p)
                      :Geometry {:Point pos}}
                     (fallback-id :cg-point i))))
    parsed)))

(defn read-catalog
  "Parsed LandXML root element → survey overlay catalog."
  [root]
  (let [root (eager-element root)
        attrs (xu/attrs-by-local root)
        warnings (atom [])
        used (atom #{})
        {:keys [index points]} (index-cg-points root)
        point-objs (emit-cg-points points used)
        line-objs (collect-linework root index warnings used)
        crs (read-coordinate-system root)]
    (cond-> {:schemacraft/format :landxml-survey-geometry
             :landxml/version (or (not-empty (str/trim (str (or (get attrs "version") ""))))
                                  "1.2")
             :warnings (vec @warnings)
             :objects (into point-objs line-objs)}
      crs (assoc :coordinate-system crs))))

(defn read-file
  "Parse LandXML from a data.xml-compatible source (stream/reader) → catalog."
  [source]
  (read-catalog (xu/parse-xml source)))

(defn read-file-path
  [path]
  (with-open [in (io/input-stream path)]
    (read-file in)))
