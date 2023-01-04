(ns aoc.day18
  (:require [aoc.utils :as u]))

(defn- parse [input]
  (->> input
       (re-seq #"\d+")
       (map parse-long)
       (partition 3)
       (map vec)
       set))

(defn around-cube [c]
  (map #(u/+p c %) [[0 0 1] [0 0 -1] [0 1 0] [0 -1 0] [1 0 0] [-1 0 0]]))

(defn- surface-area [cubes]
  (apply + (for [c cubes] (count (filter #(not (contains? cubes %)) (around-cube c))))))

(defn- extent [cubes dim]
  [(dec (reduce min (map #(% dim) cubes)))
   (inc (reduce max (map #(% dim) cubes)))])

(defn- exterior-area [cubes]
  (let [[min-x max-x] (extent cubes 0)
        [min-y max-y] (extent cubes 1)
        [min-z max-z] (extent cubes 2)]
    (loop [queue (u/queue [[min-x min-y min-z]]), seen #{}, area 0]
      (if-let [c (peek queue)]
        (let [front (filter #(and (<= min-x (c 0) max-x)
                                  (<= min-y (c 1) max-y)
                                  (<= min-z (c 2) max-z)
                                  (not (contains? seen %))
                                  (not (contains? cubes %)))
                            (around-cube c))]
          (recur (into (pop queue) front)
                 (into seen front)
                 (->> (around-cube c) (filter #(contains? cubes %)) count (+ area))))
        area))))

(defn- solution [input]
  (let [cubes (parse input)]
    [(surface-area cubes) (exterior-area cubes)]))

(u/solve 18)
(u/add-solution 18 solution)