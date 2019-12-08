(ns aoc2019-clj.day8
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combs]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [aoc2019-clj.utils.utils :as utils]))

(def input (slurp (io/resource "day8.txt")))
(def numstring (str/trim-newline input))

(defn make-layers [input width height]
  (mapv (partial partition width)
        (partition (* width height) input)))

(def layers (make-layers input 25 6))

(def winner
  (apply min-key
         (fn [layer] (count (mapcat #(filter (partial = \0) %) layer)))
         layers))

(def ones (count (mapcat #(filter (partial = \1) %) winner)))
(def twos (count (mapcat #(filter (partial = \2) %) winner)))
;; part one
(* ones twos)

;; part two
(defn assign-color [& slices]
  (apply map
         #(or (some #{ \1 \0} %&) \2)
         slices))

(def res (apply map assign-color layers))
(map println (partition 25 (map #(if (= \1 %) "*" " " ) (flatten res))))
