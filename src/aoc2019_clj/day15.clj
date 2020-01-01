(ns aoc2019-clj.day13
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combs]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [aoc2019-clj.utils.utils :as utils]
            [aoc2019-clj.intcode :refer [interpret make-program get-op] :as intcode]
            [clojure.core.async :refer [go >! <! >!! <!! chan alts! alts!!]]))

(def instructions (->>  "day15.txt" io/resource slurp))
(def program (make-program instructions))
;;inputs 1-4 nswe
;;outputs 0-2 wall, moved 1 step in dir,moved and now at oxygen

(defn get-output [p]
  (some (fn [p]
          (cond (:halted? p) [:halted p]
                (some? (:output p)) [(:output p) p]
                :else nil))
        ;; rest otherwise keep taking val of prog before
        ;; iterate actually called to change prog
        (rest p)))

(defn collect [program input]
  (when-not (:halted? program)
    (let [p (iterate #((make-interpreter program) % %) program)
          [out newp] (when (get-output p) (get-output p))]
      (when (not= out :halted)
        (lazy-seq (cons out (collect newp input)))))))

;; concerns - does calling collect 4 times at each turn put the program
;; in a state where the pointer has advanced too far and we should have
;; used a fresh copy of the program state for each attempt?

(def move->dir {1 [-1 0] 2 [1 0] 3 [0 -1] 4 [0 1]})

(defn check-moves []
  (map
   #(vector % (first (collect @intcode/program %)))
   (range 1 5)))

(defn next-moves [pos]
  (for [[direction result] (check-moves)
        :when (not= result 0)]
    (if (= result 2)
      (do (println "all done") [:done])
      (mapv + pos (get move->dir direction)))))

(defn breadth-first-search [start next-moves is-goal?]
  (let [q (clojure.lang.PersistentQueue/EMPTY)]
    (loop [open (conj q start)
           closed #{}]
      (cond (is-goal? (last (peek open))) (peek open)
            (closed (peek open)) (recur (pop open) closed)
            :else (recur (into (pop open)
                               (map #(conj (peek open) %)
                                       (next-moves (last (peek open)))))
                         (conj closed (peek open)))))))

(breadth-first-search [[0 0]] next-moves #(= % [:done]))

#_(breadth-first-search
 [[0 0]]
 #(mapv (partial  mapv + %) [[1 1] [-1 -1] [1 -1] [-1 1] [0 -1] [0 0] [-1 0]])
 #(= % [1 6]))
