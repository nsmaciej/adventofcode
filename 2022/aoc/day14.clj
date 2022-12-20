(ns aoc.day14
  (:require [aoc.utils :as u]
            [clojure.string :as str]))

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

(defn- trace [f limit cave p]
  (if (> (first p) limit)
    {:done cave}
    (reduce (fn [cave p*]
              (if (contains? cave p*)
                cave
                (let [r (trace f limit cave p*)]
                  (if (:done r) (f r) r))))
            (assoc! cave p :sand)
            (map #(u/+p p %) [[1 0] [1 -1] [1 1]]))))

(defn- solution [input]
  (let [cave (parse-cave input)
        max-y (:max-y (u/chart-extent cave))
        count-sand #(count (filter #{:sand} (vals %)))]
    [(-> (trace reduced (dec max-y) (transient cave) [0 500])
         :done
         persistent!
         count-sand
         (- max-y))
     (-> (trace :done (inc max-y) (transient cave) [0 500])
         persistent!
         count-sand)]))

(u/add-solution 14 solution)