(ns aoc2019-clj.core
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combs]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [aoc2019-clj.utils.utils :as utils]))

(def day1-data (slurp (io/resource "day1.txt")))
(combs/subsets ["a" "b" "c"])
(utils/a-star-search )
