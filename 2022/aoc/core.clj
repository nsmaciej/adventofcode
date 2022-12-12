(ns aoc.core
  (:require [aoc.utils :as u]
            [clojure.tools.cli :refer [parse-opts]]
            [criterium.core :as criterium]
            aoc.day01 aoc.day02 aoc.day03 aoc.day04
            aoc.day05 aoc.day06 aoc.day07 aoc.day08
            aoc.day09 aoc.day10 aoc.day11))

(def cli-options
  [["-d" "--day DAY" "Day number"
    :parse-fn parse-long
    :validate [#(<= 1 % 24) "Must be a valid day 1-24"]]
   ["-t" "--time" "Time the solutions"]
   ["-q" "--quick" "Quick-bench the solutions"]])

(defn- run-day [day & {:keys [time quick]}]
  (printf "=== Day %d ===\n" day)
  (let [input (u/input day)
        solution (@u/solutions day)]
    ;; Use vec on the solution to make sure we are not timing
    ;; how long it takes to build a lazy list.
    (if quick
      (criterium/quick-bench (vec (solution input)))
      (let [start (System/nanoTime)
            result (vec (solution input))
            end (System/nanoTime)]
        (run! println result)
        (when time
          (printf "Took %.2f ms\n" (/ (- end start) 1000000.0)))))))

(defn -main [& args]
  (let [{:keys [options errors]} (parse-opts args cli-options)]
    (when errors
      (run! println errors)
      (System/exit 1))
    (if-let [day (:day options)]
      (run-day day options)
      (run! #(run-day % options) (keys @u/solutions)))
    (shutdown-agents)))