(ns aoc.day03
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def item->priority
  (zipmap "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" (range 1 57)))

(defn- find-common [[lhs rhs]]
  (first (set/intersection (set lhs) (set rhs))))

(defn- part1 [data]
  (->> data
       (map #(split-at (quot (count %) 2) %))
       (map find-common)
       (map item->priority)
       (reduce +)))

  ; Part 2
(defn- part2 [data]
  (->> data
       (partition 3)
       (map #(first (apply set/intersection (map set %))))
       (map item->priority)
       (reduce +)))

(defn solution [input]
  (let [data (str/split-lines input)]
    [(part1 data) (part2 data)]))
