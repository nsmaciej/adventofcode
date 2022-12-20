(ns aoc.day14
  (:require [aoc.utils :as u]
            [clojure.string :as str]))

(defn- cave-str [cave]
  (u/chart-str \. (update-vals cave {:rock \#
                                     :sand \o})))

(defn- trace-path [start end]
  (let [delta (mapv u/sign (u/-p end start))]
    (loop [r [], p start]
      (if (= p end) (conj r p) (recur (conj r p) (u/+p p delta))))))

(defn- parse-path [s]
  (eduction (map parse-long)
            (partition-all 2)
            (map u/transposep)
            u/sliding-pair
            (mapcat #(trace-path (first %) (second %)))
            (str/split s #",| -> ")))

(defn- parse-cave [s]
  (zipmap (mapcat parse-path (str/split-lines s))
          (repeat :rock)))

(def directions [[1 0] [1 -1] [1 1]])

(defn- trace-sand [limit cave start]
  (if (>= (first start) limit)
    (reduced cave)
    (reduce (fn [c d] (if (contains? c (u/+p start d)) c
                          (let [r (trace-sand limit c (u/+p start d))]
                            (if (reduced? r) (reduced r) r))))
            (assoc cave start :sand) directions)))

(defn- trace-sand [limit cave start]
  (if (>= (first start) limit)
    (reduced cave)
    (reduce (fn [c d] (if (contains? c (u/+p start d)) c
                          (let [r (trace-sand limit c (u/+p start d))]
                            (if (reduced? r) (reduced r) r))))
            (assoc cave start :sand) directions)))

(def sample "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9")

(let [cave (parse-cave sample)
      limit (reduce max (map first (keys cave)))]
  (- (count (filter #{:sand} (vals @(trace-sand limit cave [0 500])))) limit))