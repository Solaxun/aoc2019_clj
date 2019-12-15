(ns aoc2019-clj.intcode
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combs]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [aoc2019-clj.utils.utils :as utils]))

(def input (slurp (io/resource "day9.txt")))

(def program-init-state
  (-> input (str/split #",")
      ((partial mapv read-string))))

(def program
  (atom {:memory (into {} (map-indexed vector program-init-state))
         :pointer 0
         :base 0
         :inputs-received 1
         :halted? false
         :output []}))

(defn get-op [op]
  (if (= 99 op)
    99
    (->> op str last str read-string)))

(defn get-modes [op]
  (let [opcode (get-op op)
        nargs ({1 3 2 3 3 1 4 1 5 2 6 2 7 3 8 3} opcode)]
    (reverse (concat (repeat (- nargs (count (drop-last 2 (str op)))) 0)
                     (->> op str (drop-last 2) (map #(-> % str read-string)))))))

(defn arg-modes
  [memory pointer]
  (let [op (memory pointer)
        args (map (partial get memory)
                  (range (inc pointer) (+ 4 pointer)))]
    (map vector args (get-modes op))))

(defn mem-get
  [{:keys [memory pointer base inputs-received halted? output]}
   k mode]
  (println "in memget")
  (case mode
    0 (get memory k 0)
    1 k
    2 (get memory (+ k base) 0)))

(defn mem-set
  [{:keys [memory pointer base inputs-received halted? output]}
   k v mode]
  (let [k (case mode
            0 k
            2 (+ k base))])
  (swap! program update :memory assoc k v))

(defn add
  [{:keys [memory pointer base inputs-received halted? output]}
   x y z zmode]
  (swap! program #(-> %
                      (update :pointer + 4)
                      (mem-set z (+ x y) zmode))))

(defn mult
  [{:keys [memory pointer base inputs-received halted? output]}
   x y z zmode]
  "in mult"
  (swap! program #(-> %
                      (update :pointer + 4)
                      (mem-set z (* x y) zmode))))

(defn receive
  [{:keys [memory pointer base inputs-received halted? output]}
   x xmode phase input]
  (swap! program #(-> %
                      (update :inputs-received inc)
                      (update :pointer + 2)
                      (mem-set x (if (= 1 inputs-received) phase input) xmode))))

(defn -send
  [{:keys [memory pointer base inputs-received halted? output]} x]
  (do (println x)
      (swap! program #(-> %
                          (update :pointer + 2)
                         (update :output conj x)))))

(defn jump-if-true
  [{:keys [memory pointer base inputs-received halted?]} x y]
  (swap! program assoc :pointer (if (not= 0 x) y pointer)))

(defn jump-if-false
  [{:keys [memory pointer base inputs-received halted?]} x y]
  (swap! program assoc :pointer (if (zero? x) y pointer)))

(defn store-if-lt
  [{:keys [memory pointer base inputs-received halted?]} x y z zmode]
  (swap! program #(-> %
                      (update :pointer + 4)
                      (mem-set z (if (< x y) 1 0) zmode))))

(defn store-if-eq
  [{:keys [memory pointer base inputs-received halted?]} x y z zmode]
  (swap! #(-> %
              (update :pointer + 4)
              (mem-set z (if (= x y) 1 0) zmode))))

(defn interpret [program phase input]
  (let [{:keys [memory pointer base inputs-received halted? output]} @program]
    (println "pointer: " pointer "phase: " phase "halted?: " halted?
             "op" (get-op (get memory pointer)) "argmodes" (arg-modes memory pointer))
    (when-not halted?
      (let [op (get-op (get memory pointer))
            [[x xmode] [y ymode] [z zmode]] (arg-modes memory pointer)]
        #_(println x xmode y ymode z zmode)
        (case op
          1 (add program (mem-get program x xmode) (mem-get program y ymode) z zmode)
          2 (mult program (mem-get program x xmode) (mem-get program y ymode) z zmode)
          3 (receive program x xmode phase input)
          4 (-send program (mem-get program x xmode))
          5 (jump-if-true program (mem-get program x xmode) (mem-get program y ymode))
          6 (jump-if-true program (mem-get program x xmode) (mem-get program y ymode))
          7 (store-if-lt program (mem-get program x xmode) (mem-get program y ymode) z zmode)
          8 (store-if-eq program (mem-get program x xmode) (mem-get program y ymode) z zmode)
          9 (swap! program assoc :halted? true))))))

#_(take-while
 (fn [program] (not (:halted? program)))
 (iterate (fn [program] (interpret program 5 5)))
 program-init-state)
;; ((interpret program 5) 5) ;; should be 5000972

(interpret program 5 5)
(let [phase 5 input 5
      {:keys [memory pointer base inputs-received halted? output]} @program
      op (get-op (get memory pointer))
      [[x xmode] [y ymode] [z zmode]] (arg-modes memory pointer)]
  (println "code: " (get memory pointer) "op: " op "args/modes: " x xmode y ymode z zmode)
  (case op
    1 (add program (mem-get program x xmode) (mem-get program y ymode) z zmode)
    2 (mult program (mem-get program x xmode) (mem-get program y ymode) z zmode)
    3 (receive program x xmode phase input)
    4 (-send program (mem-get program x xmode))
    5 (jump-if-true program (mem-get program x xmode) (mem-get program y ymode))
    6 (jump-if-true program (mem-get program x xmode) (mem-get program y ymode))
    7 (store-if-lt program (mem-get program x xmode) (mem-get program y ymode) z zmode)
    8 (store-if-eq program (mem-get program x xmode) (mem-get program y ymode) z zmode)
    9 (swap! program assoc :halted? true)))

(mult program (mem-get program 34463338 1) (mem-get program 34463338 1) 63 0)
