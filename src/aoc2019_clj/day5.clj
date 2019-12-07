(ns aoc2019-clj.day5
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combs]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [aoc2019-clj.utils.utils :as utils]))

(def data (slurp (io/resource "day5.txt")))
(def nums (mapv read-string (str/split data #",")))

(defn mode-get [code v m]
  (if (= m 0) (code v) v))

(defn get-op [op] (->> op str last str read-string))

(defn get-modes [op]
  (let [opcode (get-op op)
        nargs ({1 3 2 3 3 1 4 1} opcode)]
    (reverse (concat (repeat (- nargs (count (drop-last 2 (str op)))) 0)
                     (->> op str (drop-last 2) (map #(-> % str read-string)))))))

(defn math [op]
  (fn [instr [[x xmode] [y ymode]]]
    (op (mode-get instr x xmode) (mode-get instr y ymode))))

(defn process [instructions input]
  (loop [i 0 instr instructions]
    (let [[op x y z & xs] (subvec instr i)]
      (when (< i (count instr))
        (case (get-op op)
          1 (recur (+ i 4) (assoc instr z ((math +) instr (map vector [x y] (get-modes op)))))
          2 (recur (+ i 4) (assoc instr z ((math *) instr (map vector [x y] (get-modes op)))))
          3 (recur (+ i 2) (assoc instr x input))
          4 (recur (+ i 2) (do (println (mode-get instr x (-> op get-modes first))) instr))
          99 instr
          instr)))))
;; part 1
(process nums 1)

;; part 2
(defn compare [cmp]
 (fn [instr [[x xmode] [y ymode]]]
  (if (cmp (mode-get instr x xmode)
           (mode-get instr y ymode))
      1
      0)))

(defn get-modes [op]
  (let [opcode (get-op op)
        nargs ({1 3 2 3 3 1 4 1 5 2 6 2 7 3 8 3} opcode)]
    (reverse (concat (repeat (- nargs (count (drop-last 2 (str op)))) 0)
                     (->> op str (drop-last 2) (map #(-> % str read-string)))))))

(defn jmp [pred]
 (fn [instr [[x xmode] [y ymode]]]
  (when (pred (mode-get instr x xmode))
    (mode-get instr y ymode))))

(defn process [instructions input]
  (loop [i 0 instr instructions]
   (let [[op x y z & xs] (subvec instr i)]
     (when (< i (count instr))
      (case (get-op op)
        1 (recur (+ i 4) (assoc instr z ((math +) instr (map vector [x y] (get-modes op)))))
        2 (recur (+ i 4) (assoc instr z ((math *) instr (map vector [x y] (get-modes op)))))
        3 (recur (+ i 2) (assoc instr x input))
        4 (recur (+ i 2) (do (println (mode-get instr x (-> op get-modes first))) instr))
        5 (recur (or ((jmp (partial not= 0))instr (map vector [x y] (get-modes op))) (+ i 3)) instr)
        6 (recur (or ((jmp (partial = 0)) instr (map vector [x y] (get-modes op))) (+ i 3)) instr)
        7 (recur (+ i 4) (assoc instr z ((compare <) instr (map vector [x y] (get-modes op)))))
        8 (recur (+ i 4) (assoc instr z ((compare =) instr (map vector [x y] (get-modes op)))))
        99 instr
        instr)))))

(process nums 5)
