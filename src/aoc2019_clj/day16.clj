(ns aoc2019-clj.day16
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combs]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [aoc2019-clj.utils.utils :as utils]))

(def input (slurp (io/resource "day16.txt")))
(def nums (mapv read-string (re-seq #"\d" input)))
(def base-pattern [0 1 0 -1])

(defn repeat-base [n base]
  (->> base
       (mapcat (partial repeat n))
       (cycle)
       (drop 1)))

(defn keep-ones [n]
  (let [sn (str n)]
    (if (> (count sn) 1)
      (-> sn last str read-string)
      n)))

(defn fft [num]
  (map-indexed
   (fn [i _] (keep-ones (reduce + (mapv * num (repeat-base (inc i) base-pattern)))))
   num))

(defn part1 [nums n]
  (take 8 (nth (iterate fft nums) n)))

(part1 nums 100)
