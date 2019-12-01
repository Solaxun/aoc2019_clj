(ns aoc2019-clj.day1
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combs]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [aoc2019-clj.utils.utils :as utils]))

(def data (slurp (io/resource "day1.txt")))
(def nums (map read-string (str/split-lines data)))

(defn mass [n] (-> n (quot 3) (- 2)))
;; part 1
(reduce + (map mass nums))

(defn recursive-mass [n]
  (reduce + (drop 1 (take-while pos? (iterate mass n)))))
;; part 2
(reduce + (map recursive-mass nums))
