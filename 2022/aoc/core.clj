(ns aoc.core
  (:require [aoc.utils :as u]
            [clojure.tools.cli :refer [parse-opts]]
            [criterium.core :as criterium]
            [aoc.day01]
            [aoc.day02]
            [aoc.day03]
            [aoc.day04]
            [aoc.day05]
            [aoc.day06]
            [aoc.day07]
            [aoc.day08]
            [aoc.day09]
            [aoc.day10]))

(def day->solution
  {1 aoc.day01/solution
   2 aoc.day02/solution
   3 aoc.day03/solution
   4 aoc.day04/solution
   5 aoc.day05/solution
   6 aoc.day06/solution
   7 aoc.day07/solution
   8 aoc.day08/solution
   9 aoc.day09/solution
   10 aoc.day10/solution})

(def cli-options
  [["-d" "--day DAY" "Day number"
    :parse-fn parse-long
    :validate [#(<= 1 % 24) "Must be a valid day 1-24"]]
   ["-t" "--time" "Time the solutions"]])

(defn- run-day [day & {:keys [time]}]
  (let [input (u/input day)
        start (System/nanoTime)
        result ((day->solution day) input)
        end (System/nanoTime)]
    (run! println result)
    (when time
      (printf "Took %.2f ms\n" (/ (- end start) 1000000.0)))))

(defn -main [& args]
  (let [{:keys [options errors]} (parse-opts args cli-options)]
    (when errors
      (run! println errors)
      (System/exit 1))
    (if-let [day (:day options)]
      (run-day day options)
      (run! #(run-day % options) (range 1 (inc (count day->solution)))))))
