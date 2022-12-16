(ns aoc.day01
  (:require [clojure.string :as str]
            [aoc.utils :as u]))

(defn- solution [input]
  (let [[a b c] (sort > (for [group (str/split input #"\n\n")]
                          (->> group
                               str/split-lines
                               (map parse-long)
                               (apply +))))]
    [a (+ a b c)]))

(u/add-solution 1 solution)