(ns aoc2019-clj.day22
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combs]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [aoc2019-clj.utils.utils :as utils]
            [aoc2019-clj.intcode :refer [interpret make-program get-op] :as intcode]
            [clojure.core.async :refer [go >! <! >!! <!! chan alts! alts!!]]))

(def instructions (->>  "day22.txt" io/resource slurp str/split-lines))

(defn deal-in [cards]
  (reverse cards))

(defn cut-stack [cards n]
  (if (neg? n)
    (apply concat (reverse (split-at (- (count cards) (Math/abs n)) cards)))
    (apply concat (reverse (split-at n cards)))))

(defn deal-with-increment [cards n]
  (loop [new-cards (vec cards)
         old-cards cards
         i 0]
    (if old-cards
      (recur (assoc new-cards
                    (mod i (count cards))
                    (first old-cards))
             (next old-cards)
             (+ i n))
      new-cards)))

(defn parse-instruction [instruction]
  (let [num (last (str/split instruction #" "))]
    (cond (re-matches #"deal into .*" instruction)
          #(deal-in %)

          (re-matches #"deal with .*" instruction)
          #(deal-with-increment % (read-string num))

          :else
          #(cut-stack % (read-string num)))))

(filter some?
        (map-indexed (fn [i card] (when (= card 2019) i))
                     (reduce (fn [cards instruction]
                               ((parse-instruction instruction) cards))
                             (vec (range 10007))
                             instructions)))
