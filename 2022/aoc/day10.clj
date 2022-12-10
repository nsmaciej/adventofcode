(ns aoc.day10
  (:require [clojure.string :as str]))

(defn- simulate [x-value line]
  (let [[inst op] (str/split line #" ")]
    (case inst
      "addx" [x-value (+ x-value (parse-long op))]
      "noop" [x-value])))

(defn- part-1 [x-values]
  (let [steps [20 60 100 140 180 220]]
    (apply + (map #(* % (nth x-values (dec %))) steps))))

(defn- part-2 [x-values]
  (->> (map #(if (<= -1 (- %1 (mod %2 40)) 1) \# \space)
            x-values
            (range (* 40 60)))
       (partition 40)
       (map str/join)
       (str/join "\n")))

(defn solution [input]
  (let [x-values (->> input
                      str/split-lines
                      (reductions #(simulate (last %1) %2) [1])
                      (apply concat)
                      vec)]
    [(part-1 x-values), (part-2 x-values)]))