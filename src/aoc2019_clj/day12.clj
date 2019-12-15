(ns aoc2019-clj.day12
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combs]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [aoc2019-clj.utils.utils :as utils]
            [aoc2019-clj.day9 :refer [process]]))

#_(def input  (slurp (io/resource "day12.txt")))
  (def input "<x=-1, y=0, z=2>
  <x=2, y=-10, z=-7>
  <x=4, y=-8, z=8>
  <x=3, y=5, z=-1>")

(def positions
  (->> input
      (re-seq #"-?\d+")
      (map read-string)
      (partition 3)))

(def moons
  (map #(take 6 (concat % (repeat 0)))
    positions))

(defn velocity [moon] (take-last 3 moon))

(defn position [moon] (take 3 moon))

(defn compare-gravity [m1 m2]
  (apply mapv #(cond (> %1 %2) -1 (= %1 %2) 0 :else 1)
    (map position [m1 m2])))

(defn update-positions [moons]
 (apply merge-with (partial mapv +)
   (for [moon1 moons
         moon2 (remove #{moon1} moons)]
    {moon1 (compare-gravity moon1 moon2)})))

(defn update-velocities [moons]
 (let [deltav (update-positions moons)]
     (map (fn [moon]
            (let [new-velocity (mapv + (deltav moon) (velocity moon))]
              (concat (mapv + new-velocity (position moon)) new-velocity) ))
      moons)))

(defn absolute-sum [selector moon]
 (reduce + (map #(Math/abs %) (selector moon))))

(defn total-energy [moon]
 (* (absolute-sum position moon) (absolute-sum velocity moon)))

(defn part1 []
  (->> moons
       (iterate update-velocities)
       (#(nth % 1000))
       (map total-energy)
       (reduce +)))

(defn transpose [v] (apply map vector v))

(defn get-cycles [n]
  (let [seen (into {} (map-indexed vector (transpose moons)))]
    (loop [states (rest (iterate update-velocities moons))
           i 0
           moon-cycles {}]
      (let [moons (first (rest states))
            axes (map-indexed vector (transpose moons))
            repeats (filter (fn [[i ax]] (= ax (seen i))) axes)]
        #_(println moon-cycles)
        (cond (or (moon-cycles n) #_(> i 400) #_(= 6 (count moon-cycles)))
              (moon-cycles n)
              #_(vals moon-cycles)

              (not (empty? repeats))
              (recur (drop 1 states)
                     (inc i)
                     (into moon-cycles (map vector (map first repeats) (repeat i))))
              :else
              (recur (drop 1 states)
                     (inc i)
                     moon-cycles))))))
(map get-cycles (range 6))
