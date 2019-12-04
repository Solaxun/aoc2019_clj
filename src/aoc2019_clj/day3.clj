(ns aoc2019-clj.day3
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combs]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [aoc2019-clj.utils.utils :as utils]))

(def data (slurp (io/resource "day3.txt")))

(def wire1 (str/split (first (str/split data #"\n")) #","))
(def wire2 (str/split (nth (str/split data #"\n") 1) #","))

(defn manhattan-distance [pt1 pt2]
  (reduce + (mapv (fn [x y] (Math/abs (- x y))) pt1 pt2)))

(def dir->coord
  {\U [-1 0]
   \D [1 0]
   \L [0 -1]
   \R [0 1]})

(def arbitrary-start [0 0])
(defn chars-to-num [n] (-> (apply str n) read-string))

(defn vector-add [p1 p2] (mapv + p1 p2))

(defn move [pos [dir & num-moves]]
  (take (chars-to-num num-moves)
        (rest (iterate (partial vector-add (dir->coord dir)) pos))))

;; get all intersections (remove start)
;; sort by manhattan that isn't 0

;; intersection means location seen, but not in own locations
;; set intersection

(defn step-move [[pos curwire] instr]
  #_(println curwire)
  (let [m (move pos instr)
        p (last m)]
    [p (concat curwire m)]))

(reduce step-move
        [arbitrary-start []]
        wire1)

(def wireset1 (-> (reductions step-move [arbitrary-start []] wire1)
                  last
                  last
                  set))

(def wireset2 (-> (reductions step-move [arbitrary-start []] wire2)
                  last
                  last
                  set))

(def intersections (clojure.set/intersection wireset1 wireset2))

;; part 1 - first value
(map (partial manhattan-distance arbitrary-start)
     (sort-by (partial manhattan-distance arbitrary-start) intersections))

;; part 2 - need each list to be in intersection
(def wiremap-a (zipmap (-> (reductions step-move [arbitrary-start []] wire1)
                           last
                           last)
                       (iterate inc 1)))

(def wiremap-b (zipmap (-> (reductions step-move [arbitrary-start []] wire2)
                           last
                           last)
                       (iterate inc 1)))


(sort (mapv (fn [isxn]
              (+ (get wiremap-a isxn) (get wiremap-b isxn)))
            intersections))

(-> (reductions step-move [arbitrary-start []] wire1)
    last ;final pos and path
    last ;final path only
    set)

(def wirea1
  (str/split "R75,D30,R83,U83,L12,D49,R71,U7,L72,U62,R66,U55,R34,D71,R55,D58,R83" #","))
(def wirea2
  (str/split "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51,U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" #","))
(defn wiremap [wirestring]
  (zipmap-firsts (-> (reductions step-move [arbitrary-start []] wirestring)
              last
              last)
          (iterate inc 1)))

(def isects (clojure.set/intersection (-> (wiremap wirea1) keys set)
                                      (-> (wiremap wirea2) keys set)))

(sort (mapv (fn [isxn]
              (+ (get (->  wirea1 wiremap) isxn)
                 (get (->  wirea2 wiremap) isxn)))
            isects))
(count (wiremap wirea1)) ;; don't override if already in map
(reduce + (map #(-> % rest chars-to-num) wirea1))
(sort (wiremap wirea1))

(defn zipmap-firsts [xs ys]
  (reduce (fn [m [k v]] (if (m k) m (assoc m k v)))
          {}
          (map vector xs ys)))
