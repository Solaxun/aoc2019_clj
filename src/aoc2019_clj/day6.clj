(ns aoc2019-clj.day6
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combs]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [aoc2019-clj.utils.utils :as utils]))

(def input (slurp (io/resource "day6.txt")))

(def from->to
 (mapv #(str/split % #"\)")
        (str/split-lines input)))

(defn multimap->adjlist [mmap]
 (reduce (fn [adjacency-list [from to]]
   (update adjacency-list from (fnil conj []) to))
   {}
   mmap))

(def orbits (multimap->adjlist from->to))

(defn path-combiner [path]
  (map #(concat path [%]) (orbits (last path))))

;; part 1
(->> [["COM"]]
     (iterate (fn [paths] (mapcat path-combiner paths)))
     (take-while (comp seq first))
     (mapcat vec)
     (map (comp dec count))
     (reduce +))
;; part 2
(defn invert-adjlist [adjlist]
 (reduce-kv
  (fn [m k v] (reduce #(update %1 %2 (fnil conj []) k) m v))
  {}
  adjlist))

(defn breadth-first-search [start next-moves is-goal?]
  (let [q (clojure.lang.PersistentQueue/EMPTY)]
    (loop [open (conj q [0 start])
           closed #{}]
           #_(println (peek open))
      (cond (empty? open) closed
            (is-goal? (peek open)) (peek open)
            (closed (second (peek open))) (recur (pop open) closed)
            :else (recur (into (pop open) (next-moves (peek open)))
                         (conj closed (second (peek open))))))))

(defn expand [[cnt node]]
  (mapv (fn [child] [(inc cnt) child])
        ((merge-with (comp vec concat) orbits (invert-adjlist orbits))
         node)))

(breadth-first-search
  "SSR"
  expand
  (fn [[i node]] (= node "D4B")))
