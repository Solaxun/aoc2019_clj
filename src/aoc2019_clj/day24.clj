(ns aoc2019-clj.day24
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combs]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [aoc2019-clj.utils.utils :as utils]
            [aoc2019-clj.intcode :refer [make-interpreter make-program get-op] :as intcode]
            [clojure.core.async :refer [go >! <! >!! <!! chan alts! alts!!]]))

(def input (->>  "day24.txt" io/resource slurp))
(def input
"....#
#..#.
#..##
..#..
#....")

(def grid (mapv vec (str/split-lines input)))

;;;;;;;; part 1
(defn neighbor-locs [loc]
  (map #(map + loc %) [[0 1] [1 0] [0 -1] [-1 0]]))

(defn neighbors [grid loc]
  ;; if neighbor oob, assume "." or dead
  (map #(get-in grid % \.) (neighbor-locs loc)))

(defn mutate [cell neighbors]
  (let [dead (count (filter #(= % \.) neighbors))
        alive (count (filter #(= % \#) neighbors))]
    (if (= cell \.)
      (if (or (= alive 2) (= alive 1)) \# \.)
      (if (= alive 1) \# \.))))

(defn generation [grid]
  (for [[r row] (map-indexed vector grid)]
    (for [[c cell] (map-indexed vector row)
          :let [neighbors (neighbors grid [r c])]]
      (mutate cell neighbors))))

(defn gen [grid] (mapv vec (generation grid)))

(defn bio-rating [grid]
  (reduce + (map-indexed #(if (= %2 \#) (Math/pow 2 %1) 0) (flatten grid))))

(loop [g grid
       seen? #{}
       i 0]
  (if (seen? g) (-> g bio-rating bigint) (recur (gen g) (conj seen? g) (inc i))))

;;;;;;; part 2
(def middle [2 2])

(defn make-empty-grid []
  (mapv (fn [_] (vec (repeat 5 \.))) (range 5)))

(def parent->child
 {[1 2] [[0 0] [0 1] [0 2] [0 3] [0 4]]
  [2 3] [[0 4] [1 4] [2 4] [3 4] [4 4]]
  [3 2] [[4 0] [4 1] [4 2] [4 3] [4 4]]
  [2 1] [[0 0] [1 0] [2 0] [3 0] [4 0]]})

(defn neighbor-locs [loc]
 (mapv #(mapv + % loc) [[0 -1] [0 1] [1 0] [-1 0]]))

(def layers
  (->> (make-empty-grid)
       (repeat 14)
       (vec)
       (#(assoc % 7 grid))))

(defn oob? [[row col]]
  (or (> row 4) (< row 0) (> col 4) (< col 0)))

(defn get-parent-neighbor [loc nloc layers layers-n]
 (let [parent-neighbor (case (mapv - nloc loc)
                         [-1 0] [1 2]
                         [0 1]  [2 3]
                         [1 0] [3 2]
                         [0 -1] [2 1])]
   (get-in (layers (dec layers-n)) parent-neighbor)))

(defn get-child-neighbors [loc layers layers-n]
 (map (fn [[r c]] (get-in (layers (inc layers-n)) [r c]))
      (parent->child loc)))

(defn update-cell [cell neighbors]
 (let [alive (count (filter #(= % \#) neighbors))]
  (if (= cell \.)
    (if (or (= alive 1) (= alive 2)) \# \.)
    (if (= alive 1) \# \.))))

(defn process-cell [grid loc layers layers-n]
 (update-cell
  (get-in grid loc)
  (mapcat
    (fn [nloc]
      (cond (= nloc middle) (get-child-neighbors loc layers layers-n)
            (oob? nloc) (vector (get-parent-neighbor loc nloc layers layers-n))
            :else (vector (get-in grid nloc))))
    (neighbor-locs loc))))

(defn update-grid [grid layers layers-n]
  (vec (map-indexed
        (fn [i row] (vec (map-indexed
                         (fn [j cell] (if (= middle [i j])
                                       \.
                                       (process-cell grid [i j] layers layers-n)))
                     row)))
    grid)))

(defn generation [layers]
 (loop [i 1
        new-layers layers]
  (if (< i (dec (count layers)))
    (let [grid (layers i)]
      ;; update curr grid based on original layers, not new layers
      (recur (inc i)
             (assoc new-layers i (update-grid grid layers i))))
    new-layers)))

(doseq [grid (nth (iterate generation layers) 10)]
  (println "\n")
  (doseq [g grid] (println g)))
;;(update-grid grid [(make-empty-grid) (make-empty-grid) (make-empty-grid)] 10)

(count (filter #(= % \#)
               (flatten (nth (iterate generation layers) 10))))
