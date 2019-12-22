(ns aoc2019-clj.day13
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combs]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [aoc2019-clj.utils.utils :as utils]
            [aoc2019-clj.intcode :refer [interpret make-program get-op] :as intcode]
            [clojure.core.async :refer [go >! <! >!! <!! chan alts! alts!!]]))

(def instructions (->>  "day13.txt" io/resource slurp (re-seq #"-*\d+") (mapv read-string)))
(def program (make-program instructions))

(defn draw [tile]
  (case tile
    0 " "
    1 "W"
    2 "*"
    3 "_"
    4 "o"))

;; (defn get-output [p]
;;   (some (fn [p]
;;           (cond (:halted? p) [:halted p]
;;                 (some? (:output p)) [(:output p) p]
;;                 :else nil))
;;         ;; rest otherwise keep taking val of prog before
;;         ;; iterate actually called to change prog
;;         (rest p)))

;; (defn collect [program input]
;;   (let [p (iterate #(interpret % input) program)
;;         [out newp] (when (get-output p) (get-output p))]
;;     (when (not= out :halted)
;;       (lazy-seq (cons out (collect newp input))))))

(defn xy-bounds [locs]
  (apply map (fn [& xs] (apply (juxt min max) xs)) locs))


(defn empty-board [row col]
  (vec (repeat row (vec (repeat col nil)))))

(defn populate-board [empty-b loc->tiles]
  (reduce
   (fn [b [loc tile]] (assoc-in b loc (draw tile)))
   empty-b
   loc->tiles))

(defn transpose [xs] (apply map vector xs))

(defn game-board [moves]
  "moves is a vector of 3"
  (transpose (populate-board (empty-board 38 21) moves)))

(defn display-game [board]
  (doseq [b board] (println b))
  (println "\n"))

(defn outputs->tiles [outputs]
 (reduce
  (fn [m [x y tile]]
     (if (and (= x -1) (= y 0))
       (println "score-> " tile)
       (assoc m [x y] tile)))
  {}
  (partition 3 outputs)))

(def score (atom 0))

(defn advance-game [game-state board-updates]
  (let [updates (partition 3 board-updates)]
  (reduce (fn [board [x y tile]]
            (if (= [x y] [-1 0])
                ;; assoc part is a hack to make sure we still have a board
                ;; in the next iteration of reduce, otherwise the swap returns
                ;; a number - fix later to be less hacky
                (do #_(println @score) (reset! score tile) (assoc board [0 0] 0))
                (assoc board [x y] tile)))
           game-state
           (partition 3 board-updates))))

(defn move-joystick [game-state]
 (let [{ball "o" paddle "_"}
       (into {}
         (for [[loc tile] game-state
           :when (or (= tile 4) (= tile 3))]
           [(draw tile) loc]))
       [xball yball] ball
       [xpad ypad] paddle]
       ;(println ball paddle (compare xball xpad))
       (compare xball xpad))) ;; -1 if xball smaller

;;;; next two defs are re-def'd from above to stop and wait
;;;; for inputs, not just stop on halt
(defn get-output [p]
  (if (:halted? (first p)) (println "fuck you" (rest p))) ;; rest is not nil, but CONTAINS nil.  This is the problem
  (some (fn [p]
          (cond (:halted? p) [:halted p]
                (= (get-op (get-in p [:memory (:pointer p)])) 3) [:feed-me p]
                (some? (:output p)) [(:output p) p]
                :else nil))
        ;; rest otherwise keep taking val of prog before
        ;; iterate actually called to change prog
        (rest p)))

(defn collect [program input]
  (when-not (:halted? program)
    (let [p (iterate #(interpret % input) program) ;; here is problem final call to collect when halted
          [out newp] (get-output p)]
      (if (= out :halted) (println "FUFJKL"))
      (when (and (some? out) (not= out :halted) (not= out :feed-me))
        (lazy-seq (cons out (collect newp input)))))))

;; realize this lazy seq now w/ output->tiles or later on program atom
;; will have changed underneath it!
(def start-state (outputs->tiles (collect program nil)))

;; now reset program after collecting start state so subsequent calls to
;; collect have a fresh copy of program to swap
(def program (make-program (assoc instructions 0 2)))

(loop [game-state start-state]
  (when-let [moves (collect @intcode/program (move-joystick game-state))]
    (if (= (last moves) 12338) (println "done" (:halted? @intcode/program)))
    (recur (advance-game game-state moves))))

(println @score)
(rest (iterate #(interpret %  @intcode/program) nil))
(first (iterate #(interpret %  @intcode/program) nil))
;; finally - calling iterate with nil on the last call
