(ns aoc.day01
  (:require [clojure.string :as str]))

(defn parse [input]
  (map #(map parse-long (str/split-lines %))
       (str/split input #"\n\n")))

(defn solve [data]
  (let [[a b c] (sort > (map #(reduce + %) data))]
    [a (+ a b c)]))

(defn -main []
  (println (solve (parse (slurp "inputs/day01.txt")))))
