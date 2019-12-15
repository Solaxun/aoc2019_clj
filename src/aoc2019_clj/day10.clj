(ns aoc2019-clj.day10
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combs]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [aoc2019-clj.utils.utils :as utils]))

(def data (-> "day10.txt" io/resource slurp))
(def input (-> data clojure.string/split-lines))

(def points
 (for [[y row] (map-indexed vector input)
       [x el] (map-indexed vector row)
       :when (= el \#)]
   [x y]))

(defn cartesian->polar [[x y]]
  [(Math/sqrt (+ (* x x) (* y y)))
   ;; flip (atan y x) to measure angle relative to y axis, not x
   ;; negate y so [0 -4] is up not down
  (-> (Math/toDegrees (Math/atan2 x (- y)))
      (+ 360)
      (mod 360))])

(defn polar->cartesian [[r a]]
  (mapv #(-> % Math/rint  int)
        [(* (Math/sin (Math/toRadians a)) r)
         (* -1 (Math/cos (Math/toRadians a)) r)]))

(defn align-points-origin [[x y] points]
  (let [offset (mapv * [-1 -1] [x y])]
    (mapv (partial mapv + offset) points)))

(defn angle->points [[x y :as pt]]
"for each point, find the angle between it and every other point.
 If angles are the same, group together."
  (->> points
       (remove #{pt})
       (align-points-origin pt)
       (map cartesian->polar)
       (group-by second)))

;;part 1
(def point-angles
  (map #(vector (count (angle->points %)) %) points))

(def monitoring
 (last (apply max-key first point-angles)))

;; part 2
(def disintegration-order
  (mapv (fn [[angle asteroids]] [angle (-> asteroids sort vec)])
    (sort-by
         (fn [[angle points-in-angle]] [angle])
         (angle->points monitoring))))

(defn remove-first-asteroid [asteroids n]
  (update asteroids
          (mod n (count asteroids))
          (fn [[angle asteroids]] [angle (rest asteroids)])))

(defn fire-away [asteroids]
  (loop [asteroids asteroids
         destroyed 0
         turn 0]
   (let [a (-> asteroids (get (mod turn (count asteroids))) last)]
      (cond (empty? a)
           (recur asteroids destroyed (inc turn))

           (= (dec 200) destroyed)
           {:polar (first a)
            :cartesian (polar->cartesian (first a))
            :adj-cartesian (mapv + monitoring (polar->cartesian (first a)))}

           :else
           (recur (remove-first-asteroid asteroids turn)
           (inc destroyed)
           (inc turn))))))
(println monitoring)
(fire-away disintegration-order)
