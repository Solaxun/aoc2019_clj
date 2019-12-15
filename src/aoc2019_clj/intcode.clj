(ns aoc2019-clj.intcode
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combs]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [aoc2019-clj.utils.utils :as utils]))

(def input (slurp (io/resource "day5.txt")))

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

(defn arg-modes [memory pointer]
  (let [op (memory pointer)
        args (map (partial get memory)
                  (range (inc pointer) (+ 4 pointer)))]
    (map vector args (get-modes op))))

(defn mem-get [program k mode]
  (let [memory (program :memory)
        base (program :base)]
    (case mode
      0 (get memory k 0)
      1 k
      2 (get memory (+ k base) 0))))

(defn mem-set [program k v mode]
  (let [memory (:memory program)
        base (:base program)
        k (case mode
            0 k
            2 (+ k base))]
    (update program :memory assoc k v)))

(defn add [program x y z zmode]
  (-> program
      (update :pointer + 4)
      (mem-set z (+ x y) zmode)))

(defn mult [program x y z zmode]
  (-> program
      (update :pointer + 4)
      (mem-set z (* x y) zmode)))

(defn receive [program x xmode phase input]
  (-> program
      (update :inputs-received inc)
      (update :pointer + 2)
      (mem-set x (if (= 1 (program :inputs-received))
                   phase
                   input)
               xmode)))

(defn -send [program x]
  (do (println x)
      (-> program
          (update :pointer + 2)
          (update :output conj x))))

(defn jump-if-true [program x y]
  (update program :pointer #(if (not= 0 x) y (+ 3 %))))

(defn jump-if-false [program x y]
  (update program :pointer #(if (= 0 x) y (+ 3 %))))

(defn store-if-lt
  [program x y z zmode]
  (-> program
      (update :pointer + 4)
      (mem-set z (if (< x y) 1 0) zmode)))

(defn store-if-eq
  [program x y z zmode]
  (-> program
      (update :pointer + 4)
      (mem-set z (if (= x y) 1 0) zmode)))

(defn interpret [prog phase input]
  (let [{:keys [memory pointer base inputs-received halted? output]} prog]
    #_(println "pointer: " pointer "phase: " phase "halted?: " halted?
             "op" (get-op (get memory pointer)) "argmodes" (arg-modes memory pointer))
    (when-not halted?
      (let [op (get-op (get memory pointer))
            [[x xmode] [y ymode] [z zmode]] (arg-modes memory pointer)]
        #_(println x xmode y ymode z zmode)
        (case op
          1 (swap! program add (mem-get prog x xmode) (mem-get prog y ymode) z zmode)
          2 (swap! program mult (mem-get prog x xmode) (mem-get prog y ymode) z zmode)
          3 (swap! program receive x xmode phase input)
          4 (swap! program -send (mem-get prog x xmode))
          5 (swap! program jump-if-true (mem-get prog x xmode) (mem-get prog y ymode))
          6 (swap! program jump-if-false (mem-get prog x xmode) (mem-get prog y ymode))
          7 (swap! program store-if-lt (mem-get prog x xmode) (mem-get prog y ymode) z zmode)
          8 (swap! program store-if-eq (mem-get prog x xmode) (mem-get prog y ymode) z zmode)
          9 (swap! program assoc :halted? true))))))

(take-while
 (fn [program] (not (:halted? program)))
 (iterate (fn [program] (interpret program 5 5)) @program))
;; ((interpret program 5) 5) ;; should be 5000972


#_(swap! program jump-if-true 0 53)
#_(interpret (interpret @program 5 5) 5 5)
