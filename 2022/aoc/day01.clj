(ns aoc.day01
  (:require [clojure.string :as str]))

(defn- parse [input]
  (map #(map parse-long (str/split-lines %))
       (str/split input #"\n\n")))

(defn- solve [data]
  (let [[a b c] (sort > (map #(reduce + %) data))]
    [a (+ a b c)]))

(defn solution [input]
  (solve (parse input)))
