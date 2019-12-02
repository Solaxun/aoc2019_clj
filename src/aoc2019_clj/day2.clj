(ns aoc2019-clj.day2
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combs]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [aoc2019-clj.utils.utils :as utils]))

(def data (slurp (io/resource "day2.txt")))
(def nums (mapv read-string (str/split data #",")))

(def code->op {1 + 2 *})

(defn process [nums]
  (loop [nums nums
         [opcode x y out & xs] nums]
    (if (= opcode 99)
      nums
      (recur (assoc nums out ((code->op opcode) (nums x) (nums y)))
             xs))))
;; part 1
(get  (process (assoc nums 1 12 2 2)) 0)

;; part 2
(for [noun (range 100)
      verb (range 100)
      :let [res (process (assoc nums 1 noun 2 verb))]
      :when (= (res 0) 19690720)]
  (-> noun (* 100) (+ verb)))
