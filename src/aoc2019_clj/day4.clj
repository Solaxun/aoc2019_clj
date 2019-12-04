(ns aoc2019-clj.day4
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combs]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [aoc2019-clj.utils.utils :as utils]))

n(def data (slurp (io/resource "day4.txt")))

(def pw-range (->> (str/split (str/trim data) #"-")
                   (mapv read-string)
                   ((fn [[start end]] [start (inc end)]))
                   (apply range)))

(defn adjacent-digits? [numstring]
  (some (partial apply =) (partition 2 1 numstring)))

(defn increasing? [numstring]
  (= numstring (apply str (sort numstring))))

(defn max-two-adjacent? [n]
  (let [groups (partition-by identity n)
        cnts (map count groups)]
    (when (>= (apply max cnts) 2)
      (some #{2} cnts))))

;; part 1
(->> pw-range
     (map str)
     (filter (every-pred adjacent-digits? increasing?))
     count)
;; part 2
(->> pw-range
     (map str)
     (filter (every-pred adjacent-digits? increasing? max-two-adjacent?))
     count)
