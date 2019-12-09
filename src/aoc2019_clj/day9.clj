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
    1 #_(if (> v (count code))
        (get @extended-memory v 0)
        v)v
    2 (get code (+ base v) (get @extended-memory (+ base v) 0))))

(defn assoc-safe [m k v]
  #_(println m (type m)k v)
  (if (>= k (count m))
    (do (swap! extended-memory assoc k v) m)
    (assoc m k v)))

(defn get-op [op] (if (= op 99) 99 (->> op str last str read-string)))

(defn math [op base]
  (fn [instr [[x xmode] [y ymode]]]
    (op (mode-get instr x xmode base) (mode-get instr y ymode base))))

;; part 2
(defn compare [cmp base]
 (fn [instr [[x xmode] [y ymode]]]
  (if (cmp (mode-get instr x xmode base)
           (mode-get instr y ymode base))
      1
      0)))

(defn get-modes [op]
  (let [opcode (get-op op)
        nargs ({1 3 2 3 3 1 4 1 5 2 6 2 7 3 8 3 9 2} opcode)]
    (reverse (concat (repeat (- nargs (count (drop-last 2 (str op)))) 0)
                     (->> op str (drop-last 2) (map #(-> % str read-string)))))))

(defn jmp [pred base]
 (fn [instr [[x xmode] [y ymode]]]
  (when (pred (mode-get instr x xmode base))
    (mode-get instr y ymode base))))

(defn process [instructions input]
  (loop [i 0 instr instructions base 0]
    (let [[op x y z & xs] (subvec instr i)]
      (println i op x y base)
     (when (< i (count instr))
      (case (get-op op)
        1 (recur (+ i 4) (assoc-safe instr z ((math + base) instr (map vector [x y] (get-modes op)))) base)
        2 (recur (+ i 4) (assoc-safe instr z ((math * base) instr (map vector [x y] (get-modes op)))) base)
        3 (recur (+ i 2) (assoc-safe instr x input) base)
        4 (recur (+ i 2) (do (println (mode-get instr x (-> op get-modes first) base)) instr) base)
        5 (recur (or ((jmp (partial not= 0) base) instr (map vector [x y] (get-modes op))) (+ i 3)) instr base)
        6 (recur (or ((jmp (partial = 0) base) instr (map vector [x y] (get-modes op))) (+ i 3)) instr base)
        7 (recur (+ i 4) (assoc-safe instr z ((compare < base) instr (map vector [x y] (get-modes op)))) base)
        8 (recur (+ i 4) (assoc-safe instr z ((compare = base) instr (map vector [x y] (get-modes op)))) base)
        9 (recur (+ i 2) instr (+ base (mode-get instr x (-> op get-modes first) base)))
        99 instr)))))


#_(process [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99] 1) ;; works !
;; getting is find in array, or extended if oob
;; setting is same
#_(mode-get [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99] 100 0 1)
#_(process nums 1)
#_(process [1102,34915192,34915192,7,4,7,99,0] 1)
;; @extended-memory

;; (assoc-safe
;;  [1102,34915192,34915192,7,4,7,99,0]
;;  7
;;  ((math * 0) [1102,34915192,34915192,7,4,7,99,0] (map vector [34915192 34915192] (get-modes 1102))))

;; (mode-get
;;  [1102,34915192,34915192,7,4,7,99,0]
;;  34915192 1 0)
#_(process [104,1125899906842624,99] 1)

(process nums 1)
