(ns aoc.day08
  (:require [clojure.string :as str]
            [aoc.utils :as u]))

(defn- visible [xs]
  (map > xs (reductions max -1 xs)))

(defn- visible-both [xs]
  (map #(or %1 %2) (-> xs reverse visible reverse) (visible xs)))

(defn- visible-at-k [k xs]
  (->> xs
       (partition 2 1)
       (reductions #(if (>= (first %2) k) 1 (inc %1)) 0)))

(defn- visible-at-k-both [k xs]
  (map *
       (visible-at-k k xs)
       (->> xs reverse (visible-at-k k) reverse)))

(defn- visible-at-k-grid [k grid]
  (let [left-right (map #(visible-at-k-both k %) grid)
        top-down (->> grid
                      u/transpose-grid
                      (map #(visible-at-k-both k %))
                      u/transpose-grid)]
    (u/map-grid * top-down left-right)))

(defn- parse [input]
  (u/mapv-grid #(- (int %) 48) (str/split-lines input)))

(defn- part1 [data]
  (let [left-right (map visible-both data)
        top-down (->> data
                      u/transpose-grid
                      (map visible-both)
                      u/transpose-grid)]
    (->> (u/map-grid #(or %1 %2) top-down left-right)
         (u/filter-grid identity)
         count)))

(defn- part2 [data]
  (->> (map #(visible-at-k-grid % data) (range 10))
       (apply u/map-grid #(nth %& %) data)
       flatten
       (apply max)))

(defn solution [input]
  (let [data (parse input)]
    [(part1 data) (part2 data)]))
