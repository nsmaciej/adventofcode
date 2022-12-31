(ns aoc.day15
  (:require [aoc.utils :as u]
            [clojure.math.combinatorics :as combo]))

(defn- parse [input]
  (->> (re-seq #"-?\d+" input)
       (map parse-long)
       (partition 4)
       (map (fn [[x y u v]]
              {:sensor [y x]
               :beacon [v u]
               :distance (u/manhattan [y x] [v u])}))))

(defn- impossible-range
  "Given a y axis, sensor and beacon coordinates, returns an inclusive
   range where no other beacon could exist, or nil if no such range exists."
  [y [sy sx] dist]
  (let [y-delta (u/abs- y sy)
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
       (keep #(impossible-range y (:sensor %) (:distance %)))
       sort
       (reduce merge-range [])
       (map (fn [[s e]] (inc (u/abs- s e))))
       (apply + (- (occupied-positions y data)))))

(defn- bounds-as-lines
  "Returns a sequence of lines ax+b bounding the sensor's range. Each line is
  represented by the pair [a b]."
  [[sy sx] dist]
  [[1  (- (- sy dist) sx)]       ; Right \
   [-1 (- (+ sy dist) (- sx))]   ; Right /
   [1  (- (+ sy dist) sx)]       ; Left \
   [-1 (- (- sy dist) (- sx))]]) ; Left /

(defn- line-intersection
  "Returns an intersection if it exists and lies on integer coordinates."
  [[a1 b1] [a2 b2]]
  (let [over (- b2 b1), under (- a1 a2)]
    (when (and (not= under 0) (zero? (rem over under)))
      (let [x (/ over under), y (+ (* a1 x) b1)]
        [y x]))))

(defn- outside-every-sensor? [data pt]
  (every? #(> (u/manhattan pt (:sensor %))
              (u/manhattan (:beacon %) (:sensor %)))
          data))

(defn- keep-answer [bound data line-1 line-2]
  (when-let [[y x] (line-intersection line-1 line-2)]
    (when (and (<= 0 y bound)
               (<= 0 x bound)
               (outside-every-sensor? data [y x]))
      (+ y (* x 4000000)))))

(defn- part-2 [bound data]
  ;; Note the distance is incremented so that the answer lies on the generated bounds.
  (->> (combo/combinations (mapcat #(bounds-as-lines (:sensor %) (inc (:distance %))) data) 2)
       (keep #(apply keep-answer bound data %))
       first))

(defn- solution [input]
  (let [data (parse input)]
    [(part-1 2000000 data)
     (part-2 4000000 data)]))

(u/add-solution 15 solution)