(ns aoc.day03
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def item->priority
  (zipmap "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" (range 1 57)))

(defn- find-common [group]
  (first (apply set/intersection (map set group))))

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
       (map find-common)
       (map item->priority)
       (reduce +)))

(defn solution [input]
  (let [data (str/split-lines input)]
    [(part1 data) (part2 data)]))
