(ns aoc2019-clj.day9
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combs]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [aoc2019-clj.utils.utils :as utils]))

(def data (slurp (io/resource "day9.txt")))
(def nums (mapv read-string (str/split data #",")))
(def extended-memory (atom {}))

(defn mode-get [code v m base]
  (case m
    0 (get code v (get @extended-memory v 0))
    1 v
    2 (get code (+ base v) (get @extended-memory (+ base v) 0))))

(defn assoc-safe [base mode]
 (fn [m k v]
  (let [base (if (= mode 2) base 0)
        k (+ base k)]
    (if (>= k (count m))
      (do (swap! extended-memory assoc k v) m)
      (assoc m k v)))))

(defn get-op [op] (if (= op 99) 99 (->> op str last str read-string)))

(defn math [op base]
  (fn [instr [[x xmode] [y ymode]]]
    (op (mode-get instr x xmode base) (mode-get instr y ymode base))))

(defn compare [cmp base]
 (fn [instr [[x xmode] [y ymode]]]
  (if (cmp (mode-get instr x xmode base)
           (mode-get instr y ymode base))
      1
      0)))

(defn get-modes [op]
  (let [opcode (get-op op)
        nargs ({1 3 2 3 3 1 4 1 5 2 6 2 7 3 8 3 9 1 99 1} opcode)]
    (reverse (concat (repeat (- nargs (count (drop-last 2 (str op)))) 0)
                     (->> op str (drop-last 2) (map #(-> % str read-string)))))))

(defn jmp [pred base]
 (fn [instr [[x xmode] [y ymode]]]
  (when (pred (mode-get instr x xmode base))
    (mode-get instr y ymode base))))

(defn process [instructions input]
  "oh boy... fix this mess later."
  (loop [i 0 instr instructions base 0]
    (let [[op x y z & xs] (subvec instr i)]
     (when (< i (count instr))
      (case (get-op op)
        1 (recur (+ i 4) ((assoc-safe base (-> op get-modes (nth 2))) instr z ((math + base) instr (map vector [x y] (get-modes op)))) base)
        2 (recur (+ i 4) ((assoc-safe base (-> op get-modes (nth 2))) instr z ((math * base) instr (map vector [x y] (get-modes op)))) base)
        3 (recur (+ i 2) ((assoc-safe base (-> op get-modes first)) instr x input) base)
        4 (recur (+ i 2) (do (println (mode-get instr x (-> op get-modes first) base)) instr) base)
        5 (recur (or ((jmp (partial not= 0) base) instr (map vector [x y] (get-modes op))) (+ i 3)) instr base)
        6 (recur (or ((jmp (partial = 0) base) instr (map vector [x y] (get-modes op))) (+ i 3)) instr base)
        7 (recur (+ i 4) ((assoc-safe base (-> op get-modes (nth 2))) instr z ((compare < base) instr (map vector [x y] (get-modes op)))) base)
        8 (recur (+ i 4) ((assoc-safe base (-> op get-modes (nth 2))) instr z ((compare = base) instr (map vector [x y] (get-modes op)))) base)
        9 (recur (+ i 2) instr (+ base (mode-get instr x (-> op get-modes first) base)))
        99 nil)))))

(process nums 1) ; part 1
(process nums 2) ; part 2
