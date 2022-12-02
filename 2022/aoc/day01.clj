(ns aoc.day01
  (:require [clojure.string :as str]))

(defn parse [input]
  (map #(map parse-long (str/split-lines %))
       (str/split input #"\n\n")))

(defn solve [data]
  (let [sorted (->> data (map #(reduce + %)) (sort >))]
    [(first sorted)
     (reduce + (take 3 sorted))]))

(defn -main []
  (println (solve (parse (slurp "inputs/day01.txt")))))
