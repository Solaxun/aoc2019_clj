(defproject aoc2019_clj "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [org.clojure/data.json "0.2.7"]
                 [org.clojure/core.async "0.6.532"]]
  :main ^:skip-aot aoc2019-clj.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
