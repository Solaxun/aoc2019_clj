(ns aoc2019-clj.day7
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combs]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [aoc2019-clj.utils.utils :as utils]))

(def input (slurp (io/resource "day7.txt")))
(def program (-> input (str/split #",") ((partial mapv read-string))))

(defn mode-get [code v m]
  (if (= m 0) (code v) v))

(defn get-op [op] (->> op str last str read-string))

(defn math [op]
  (fn [instr [[x xmode] [y ymode]]]
    (op (mode-get instr x xmode) (mode-get instr y ymode))))

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

(defn interpret [instructions input1 input2]
  #_(println input1 input2)
  (let [called-n (atom 0)]
    #_(println "ncalls" @called-n)
    (loop [i 0 instr instructions out :fail]
      (let [[op x y z & xs] (subvec instr i)]
        (when (< i (count instr))
          (case (get-op op)
            1 (recur (+ i 4) (assoc instr z ((math +) instr (map vector [x y] (get-modes op)))) out)
            2 (recur (+ i 4) (assoc instr z ((math *) instr (map vector [x y] (get-modes op)))) out)
            3 (recur (+ i 2) (do #_(println "in 3 ->" @called-n) (swap! called-n inc)
                                 (assoc instr x (if (= 1 @called-n) input1 input2))) out)
            4 (recur (+ i 2) instr (mode-get instr x (-> op get-modes first)))
            5 (recur (or ((jmp (partial not= 0))instr (map vector [x y] (get-modes op))) (+ i 3)) instr out)
            6 (recur (or ((jmp (partial = 0)) instr (map vector [x y] (get-modes op))) (+ i 3)) instr out)
            7 (recur (+ i 4) (assoc instr z ((compare <) instr (map vector [x y] (get-modes op)))) out)
            8 (recur (+ i 4) (assoc instr z ((compare =) instr (map vector [x y] (get-modes op)))) out) out
            9 out))))))
;; part 1
#_(apply max
       (for [p (combs/permutations (range 5))
             :let [[p1 p2 p3 p4 p5] (map #(partial interpret program %) p)]]
         (-> 0 p1 p2 p3 p4 p5)))
;;(interpret program 0 0)

;; part 2 - need to not recurse until halt this time.  whenever an output is hit
;; pause execution, save state of program, and pass output to next in the chain
;; only once the final program halts are we done.

;; each one produces output possibly multiple times before halting, and passing
;; the final halted value to the next in the chain.  The last halt is completion

(defn save-state! [state newstate]
  (swap! state merge newstate))

(defn interpret [instructions input1 input2]
  ;; can't refresh state on each call... need to reuse
  (let [state (atom {:pointer 0
                     :input instructions
                     :called-cnt 1})]
    (loop [i 0 instr instructions out :fail]
      (let [[op x y z & xs] (subvec instr i)]
        (when (< i (count instr))
          (case (get-op op)
            1 (recur (+ i 4) (assoc instr z ((math +) instr (map vector [x y] (get-modes op)))) out)
            2 (recur (+ i 4) (assoc instr z ((math *) instr (map vector [x y] (get-modes op)))) out)
            ;; resume from previous state
            3 (recur (do (swap! called-n inc)
                         (assoc (-> (load-state!) :instr)
                                x
                                (if (= 1 @called-n) input1 input2)))
                     out)
            ;; return output for next in chain and freeze state for resumption later
            4 (do (swap! state update :pointer + 2)
                  (mode-get instr x (-> op get-modes first)))
            5 (recur (or ((jmp (partial not= 0))instr (map vector [x y] (get-modes op))) (+ i 3)) instr out)
            6 (recur (or ((jmp (partial = 0)) instr (map vector [x y] (get-modes op))) (+ i 3)) instr out)
            7 (recur (+ i 4) (assoc instr z ((compare <) instr (map vector [x y] (get-modes op)))) out)
            8 (recur (+ i 4) (assoc instr z ((compare =) instr (map vector [x y] (get-modes op)))) out) out
            9 out))))))
