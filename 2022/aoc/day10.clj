(ns aoc.day10
  (:require [aoc.utils :as u]
            [clojure.string :as str]))

(defn- simulate [x-value line]
  (let [[inst op] (str/split line #" ")]
    (case inst
      "addx" [x-value (+ x-value (parse-long op))]
      "noop" [x-value])))

(defn- part-1 [x-values]
  (apply + (for [step [20 60 100 140 180 220]]
             (* step (nth x-values (dec step))))))

(defn- part-2 [x-values]
  (->> x-values
       (map-indexed #(if (<= -1 (- %2 (mod %1 40)) 1) \# \space))
       (partition 40)
       (map str/join)
       (str/join "\n")))

(defn- solution [input]
  (let [x-values (->> input
                      str/split-lines
                      (reductions #(simulate (last %1) %2) [1])
                      (apply concat)
                      vec)]
    [(part-1 x-values), (part-2 x-values)]))

(u/register 10 solution)