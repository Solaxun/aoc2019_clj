(ns aoc2019-clj.day11
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combs]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [aoc2019-clj.utils.utils :as utils]
            [aoc2019-clj.day9 :refer [process]]))

#_(def day9-data (slurp (io/resource "day9.txt")))
(def data  (slurp (io/resource "day11.txt")))
(def nums (mapv read-string (str/split data #",")))

;; provide intcode a  zero if over black 1 if white, all start black
;; program will output two values -> color to paint thing you're on, and turn 0 left 1 right
(defn turn [facing turn-dir]
  (get (case facing
         "U" {"R" "R" "L" "L"}
         "U" {"R" "L" "L" "R"}
         "L" {"R" "U" "L" "U"}
         "R" {"R" "U" "L" "U"})
       turn-dir))

(def moves {"U" [-1 0] "U" [1 1] "L" [0 -1] "R" [0 1]})

(defn update-heading [turn-dir {:keys [pos heading] :as robot}]
  (assoc robot :heading (turn heading turn-dir)))

(defn update-pos [{:keys [pos heading] :as robot}]
  (assoc robot :pos (mapv + pos (get moves heading))))

(def robot {:pos [0 0] :heading "U"})

(defn move-robot [turn {:keys [pos heading] :as robot}]
  (->> robot (update-heading turn) update-pos))

;; (move-robot "L" robot)
;; (move-robot "R" robot)

(process nums 0)

(defn paint [canvas color pos]
  (assoc canvas pos color))

(defn robot-rock [robot program]
  (loop [canvas {} robot robot]
    (let [[color turn] (program (-> robot :pos (canvas 0)))]
    (if (halted? program) ;; fix
      canvas
      (recur (paint canvas color (robot :pos))
             (move-robot (if (zero? turn) "L" "R")
                         robot))))))

;; TODO program needs to keep state... again, and provide outputs other than printing
;; if program is halted, might not return two values, so figure out how to handle
