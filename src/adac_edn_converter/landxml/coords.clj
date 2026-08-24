(ns adac-edn-converter.landxml.coords
  "LandXML coordinate helpers: N/E/Z text → ADAC X/Y/Z, pntRef lookup."
  (:require [adac-edn-converter.instance.xml-util :as xu]
            [clojure.string :as str]))

(def point-epsilon
  "Metres. Vertices closer than this are treated as coincident."
  1.0e-4)

(defn parse-doubles
  "Space-delimited numeric text → vector of doubles. Blank → []."
  [s]
  (let [t (str/trim (str (or s "")))]
    (if (str/blank? t)
      []
      (mapv #(Double/parseDouble %) (str/split t #"\s+")))))

(defn ne->xy
  "LandXML Northing Easting [Elevation] numbers → ADAC {:X easting :Y northing :Z?}."
  [nums]
  (when (>= (count nums) 2)
    (cond-> {:Y (double (nth nums 0))
             :X (double (nth nums 1))}
      (>= (count nums) 3) (assoc :Z (double (nth nums 2))))))

(defn parse-point-text
  "Parse a PointType text node (\"N E\" or \"N E Z\")."
  [s]
  (ne->xy (parse-doubles s)))

(defn parse-pnt-list
  "Parse PntList2D (dim 2) or PntList3D (dim 3) space-delimited coordinates."
  [s dim]
  (let [nums (parse-doubles s)
        step (long dim)]
    (when (and (pos? step) (>= (count nums) step))
      (mapv ne->xy (partition step nums)))))

(defn same-point?
  "True when two ADAC positions share X/Y within `point-epsilon`."
  [a b]
  (boolean
   (and (map? a) (map? b)
        (number? (:X a)) (number? (:X b))
        (number? (:Y a)) (number? (:Y b))
        (< (Math/abs (- (double (:X a)) (double (:X b)))) point-epsilon)
        (< (Math/abs (- (double (:Y a)) (double (:Y b)))) point-epsilon))))

(defn child-named
  [node local]
  (first (get (xu/children-by-name node) local)))

(defn point-from-node
  "Resolve a PointType element (Start/End/Center/Location/…) via pntRef or inline text.

  Returns {:position m :pnt-ref s-or-nil :missing-ref s-or-nil}."
  [node point-index]
  (when node
    (let [attrs (xu/attrs-by-local node)
          pref (not-empty (str/trim (str (or (get attrs "pntRef") ""))))
          inline (parse-point-text (xu/text-content node))]
      (cond
        pref
        (if-let [p (get point-index pref)]
          {:position p :pnt-ref pref}
          {:missing-ref pref})

        inline
        {:position inline}

        :else nil))))
