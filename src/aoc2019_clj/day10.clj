(ns aoc2019-clj.day10
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combs]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [aoc2019-clj.utils.utils :as utils]))

(def data (slurp (io/resource "day10.txt")))
#_(def data
  ".#..#
.....
#####
....#
...##")
(def input (-> data clojure.string/split-lines))

(def points
  (for [[y row] (map-indexed vector input)
        [x el] (map-indexed vector row)
        :when (= el \#)]
    [x y]))

(defn cartesian->polar [[x y]]
  [(Math/sqrt (+ (* x x) (* y y)))
   (Math/toDegrees (Math/atan2 y x))])

(defn origin-dist [[x y]]
  (mapv - [0 0] [x y]))

(defn align-points-origin [[x y] points]
  (let [offset (origin-dist [x y])]
    (mapv + offset points)))

(defn angle->points [[x y :as pt]]
  "for each point, find the angle between it and every other point.
  If angles are the same, group together."
  (->> points
       (remove #{pt})
       (map (partial align-points-origin pt))
       (map cartesian->polar)
       (group-by second)
       count))
;; part 1
(def point-angles
  (map #(vector (angle->points %) %) points))

;; from start, look straight up and move clockwise
(def monitoring
  (apply max-key first point-angles))

point-angles
monitoring
;; next, make sure angles are 1/0 through 359/60
;; then group by angles, sort by angles, and
;; go over the list until 200 removed from front
;; skipping empty groups as needed (all astroids)
;; destroyed.
