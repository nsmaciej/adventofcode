(ns aoc.day03
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def item->priority
  (zipmap "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
          (range 1 57)))

(defn- find-common [group]
  (->> group
       (map set)
       (apply set/intersection)
       first))

(defn- part-1 [data]
  (->> data
       (map #(split-at (quot (count %) 2) %))
       (map find-common)
       (map item->priority)
       (reduce +)))

(defn- part-2 [data]
  (->> data
       (partition 3)
       (map find-common)
       (map item->priority)
       (reduce +)))

(defn solution [input]
  (let [data (str/split-lines input)]
    [(part-1 data) (part-2 data)]))
