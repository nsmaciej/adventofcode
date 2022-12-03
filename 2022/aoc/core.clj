(ns aoc.core
  (:require [aoc.utils :as u]
            [clojure.tools.cli :refer [parse-opts]]
            [aoc.day01]
            [aoc.day02]
            [aoc.day03]))

(def day->solution {1 aoc.day01/solution
                    2 aoc.day02/solution
                    3 aoc.day03/solution})

(def cli-options
  [["-d" "--day DAY" "Day number"
    :parse-fn parse-long
    :validate [#(<= 1 % 24) "Must be a valid day 1-24"]]])

(defn- run-day [day]
  (run! println ((day->solution day) (u/input day))))

(defn -main [& args]
  (let [{:keys [options errors]} (parse-opts args cli-options)]
    (when errors
      (run! println errors)
      (System/exit 1))
    (if-let [day (:day options)]
      (run-day day)
      (run! run-day (range 1 (inc (count day->solution)))))))
