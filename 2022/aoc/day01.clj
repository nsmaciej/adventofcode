(ns aoc.day01
  (:require [clojure.string :as str]))

(defn solution [input]
  (let [[a b c] (sort > (for [group (str/split input #"\n\n")]
                          (->> group
                               str/split-lines
                               (map parse-long)
                               (apply +))))]
    [a (+ a b c)]))