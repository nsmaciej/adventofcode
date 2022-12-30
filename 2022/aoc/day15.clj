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

(defn- range-union
  "Tries to unify two ranges, returning nil if they do not overlap."
  [[s1 e1] [s2 e2]]
  (when (or (<= s1 s2 e1)
            (<= s2 s1 e2))
    [(min s1 s2) (max e1 e2)]))

(defn- merge-range [ranges r]
  (loop [[r1 & ranges] ranges, result []]
    (if r1
      (if-let [merged (range-union r1 r)]
        (into (conj result merged) ranges)
        (recur ranges (conj result r1)))
      (conj result r))))

(defn- occupied-positions
  "Returns a set of x positions occupied by sensors and beacons along a given y axis."
  [y data]
  (set (eduction (mapcat #(vector (:sensor %) (:beacon %)))
                 (filter #(= (first %) y))
                 (map second)
                 data)))

(defn- part-1 [y data]
  (let [occupied (occupied-positions y data)]
    (->> data
         (keep #(impossible-range y (:sensor %) (:beacon %)))
         sort
         (reduce merge-range [])
         (map (fn [[s e]] (- (inc (u/abs- s e))
                             (count (filter #(<= s % e) occupied)))))
         (apply +))))

(defn- solution [input]
  (let [data (parse input)]
    [(part-1 2000000 data)]))

(u/add-solution 15 solution)
