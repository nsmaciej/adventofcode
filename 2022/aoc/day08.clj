(ns aoc.day08
  (:require [clojure.string :as str]
            [aoc.utils :as u]))

(defn- visible-ground
  "Looking right, return true for each treetop that
   would be visible from the ground."
  [xs]
  (map > xs (reductions max -1 xs)))

(defn- visible-at-k
  "Looking left, return how many treetops we would see below
   k height, up the to the first treetop at exactly k."
  [k xs]
  (->> xs
       (partition 2 1)
       (reductions #(if (>= (first %2) k) 1 (inc %1)) 0)))

(defn- omni-map
  "Carefully apply transposes and reverses to apply a function that
   operates on a single row, looking left, to an entire grid."
  [combine f grid]
  (let [bidirectional-f #(map combine (f %) (-> % reverse f reverse))
        left-right (map bidirectional-f grid)
        top-down (->> grid u/transpose (map bidirectional-f) u/transpose)]
    (u/map-grid combine top-down left-right)))

(defn- parse [input]
  (u/mapv-grid #(- (int %) 48) (str/split-lines input)))

(defn- part-1 [data]
  (->> (omni-map #(or %1 %2) visible-ground data)
       flatten
       (filter identity)
       count))

(defn- part-2 [data]
  (let [cache (map (fn [k] (omni-map * #(visible-at-k k %) data)) (range 10))]
    (->> (apply u/map-grid #(nth %& %) data cache)
         flatten
         (apply max))))

(defn- solution [input]
  (let [data (parse input)]
    [(part-1 data) (part-2 data)]))

(u/register 8 solution)