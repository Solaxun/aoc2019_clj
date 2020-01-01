(ns aoc2019-clj.day9
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combs]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [aoc2019-clj.utils.utils :as utils]
            [aoc2019-clj.intcode :refer [make-interpreter make-program] :as intcode]))

(defn solve [input]
  (some
   (fn [program] (when (program :output) (program :output)))
   (iterate (fn [program] ((make-interpreter program) input input))
            (deref (make-program (slurp (io/resource "day9.txt")))))))

(solve 1)
(solve 2)
