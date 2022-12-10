(ns aoc.day10
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]))

(defn- parse-line [line]
  (let [[op arg] (str/split line #"\s+")]
    (case op
      "addx" [:addx (parse-long arg)]
      "noop" [:noop])))

(defn- parse [input]
  (map parse-line (str/split-lines input)))

(defn- simulate [states inst]
  (let [x-value (last states)]
    (match inst
      [:addx arg] [x-value (+ x-value arg)]
      [:noop]     [x-value])))

(defn- part-1 [x-values]
  (let [steps [20 60 100 140 180 220]]
    (apply + (map #(* % (nth x-values (dec %))) steps))))

(defn- part-2 [x-values]
  (->> (range (* 40 60))
       (map (fn [x t] (if (>= 1 (abs (- x (mod t 40)))) "#" " "))
            x-values)
       (partition 40)
       (map #(apply str %))
       (str/join "\n")))

(defn solution [input]
  (let [x-values (->> input
                      parse
                      (reductions simulate [1])
                      flatten
                      vec)]
    [(part-1 x-values) (part-2 x-values)]))