(ns aoc2019-clj.day11
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combs]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [aoc2019-clj.utils.utils :as utils]
            [aoc2019-clj.intcode :refer [interpret make-program get-op]]))

(def instructions  (slurp (io/resource "day11.txt")))
(def program (make-program (mapv read-string (str/split instructions #","))))

(defn snd [prog value]
  (loop [{:keys [memory pointer base inputs-received halted? output] :as p} (interpret prog value)
         res []
         prev-op nil]
    (let [op (get-op (memory pointer))]
    ;; (println pointer op output)
      (cond halted?  [(if (= prev-op 4) (conj res output) res) p]
            (= op 3) [(if (= prev-op 4) (conj res output) res) p]
            (= prev-op 4) (recur (interpret p value) (conj res output) op)
            ;; (and (= op 3) (not= 1 inputs-received)) p
            :else (recur (interpret p value) res op)))))

(defn turn [facing turn-dir]
  (get (case facing
         "U" {"R" "R" "L" "L"}
         "D" {"R" "L" "L" "R"}
         "L" {"R" "U" "L" "D"}
         "R" {"R" "D" "L" "U"})
       turn-dir))

(def moves {"U" [-1 0] "D" [1 0] "L" [0 -1] "R" [0 1]})

(defn update-heading [turn-dir {:keys [pos heading] :as robot}]
  (assoc robot :heading (turn heading turn-dir)))

(defn update-pos [{:keys [pos heading] :as robot}]
  (assoc robot :pos (mapv + pos (get moves heading))))

(def robot {:pos [0 0] :heading "U"})

(defn move-robot [turn {:keys [pos heading] :as robot}]
  (->> robot (update-heading turn) update-pos))

(defn paint [canvas color pos]
  (assoc canvas pos color))

(defn robot-rock [robot program start-tile]
 (loop [canvas {[0 0] start-tile} robot robot]
  (let [[[color turn] p] (snd program (-> robot :pos (canvas 0)))]
    (if (:halted? p)
      canvas
      (recur (paint canvas color (robot :pos))
             (move-robot (if (zero? turn) "L" "R") robot))))))

;; part 1
(count (set (robot-rock robot program 0)))

;; part 2
(def program (make-program (mapv read-string (str/split instructions #","))))
(def message (robot-rock robot program 1))
(def letters (keep (fn [[k v]] (when (= 1 v) k )) message))

(map println (reduce
              (fn [grid [row col]] (assoc-in grid [row (- col 1)] "*"))
              (vec (repeat 6 (vec (repeat 38 " "))))
              letters))
