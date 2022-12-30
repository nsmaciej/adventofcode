(ns aoc.day15
  (:require [aoc.utils :as u]))

(defn- parse [input]
  (->> (re-seq #"-?\d+" input)
       (map parse-long)
       (partition 4)
       (map (fn [[x y u v]] {:sensor [y x], :beacon [v u]}))))

(defn- impossible-range
  "Given a y axis, sensor and beacon coordinates, returns an inclusive
   range where no other beacon could exist, or nil if no such range exists."
  [y [sy sx] [by bx]]
  (let [dist (+ (u/abs- sy by) (u/abs- sx bx))
        y-delta (u/abs- y sy)
        half-span (- dist y-delta)]
    (when (< y-delta dist)
      [(- sx half-span) (+ sx half-span)])))

(defn- merge-range
  "Add range r into a vector of disjoint ranges. Must be called with increasing r."
  [ranges [s2 e2]]
  (if-let [[s1 e1] (peek ranges)]
    (if (<= s2 e1)
      (conj (pop ranges) [(min s1 s2) (max e1 e2)])
      (conj ranges [s2 e2]))
    [[s2 e2]]))

(defn- occupied-positions
  "Returns count of positions occupied by sensors and beacons along a given y axis."
  [y data]
  (->> data
       (mapcat #(vector (:sensor %) (:beacon %)))
       (keep (fn [[u v]] (when (= u y) v)))
       set
       count))

(defn- part-1 [y data]
  (->> data
       (keep #(impossible-range y (:sensor %) (:beacon %)))
       sort
       (reduce merge-range [])
       (map (fn [[s e]] (inc (u/abs- s e))))
       (apply + (- (occupied-positions y data)))))

(defn- solution [input]
  (let [data (parse input)]
    [(part-1 2000000 data)]))

(u/add-solution 15 solution)