(ns aoc.day04
  (:require [aoc.utils :as u]))

(defn- parse [input]
  (->> input
       (re-seq #"\d+")
       (map parse-long)
       (partition 4)))

(defn- full-overlap? [[a x b y]]
  (or (and (<= a b) (>= x y))
      (and (<= b a) (>= y x))))

(defn- partial-overlap? [[a x b y]]
  (and (>= x b) (>= y a)))

(defn- solution [input]
  (let [data (parse input)]
    [(count (filter full-overlap? data))
     (count (filter partial-overlap? data))]))

(u/register 4 solution)