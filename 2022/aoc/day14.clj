(ns aoc.day14
  (:require [aoc.utils :as u]
            [clojure.string :as str]))

(defn trace-path [[x y] [i j]]
  (for [n (range (min x i) (inc (max x i)))
        m (range (min y j) (inc (max y j)))]
    [n m]))

(defn- parse-path [s]
  (eduction (map parse-long)
            (partition-all 2)
            (map (fn [x y] [y x]))
            u/sliding-pair
            (mapcat #(trace-path (first %) (second %)))
            (re-seq #"\d+" s)))

(defn- parse-cave [s]
  (zipmap (mapcat parse-path (str/split-lines s))
          (repeat :rock)))

(defn- trace [stop max-y cave]
  (loop [bt [[0 500]]
         cave cave]
    (if-let [p (peek bt)]
      (if (> (first p) max-y)
        (if stop cave (recur (pop bt) cave))
        (recur (into (pop bt)
                     (comp (map #(u/+p p %))
                           (filter #(not (contains? cave %))))
                     [[1 1] [1 -1] [1 0]])
               (assoc cave p :sand)))
      cave)))

(defn- solution [input]
  (let [cave (parse-cave input)
        max-y (transduce (map first) max 0 (keys cave))
        count-sand #(count (filter #{:sand} (vals %)))]
    [(- (count-sand (trace true (dec max-y) cave)) max-y)
     (count-sand (trace false (inc max-y) cave))]))

(u/add-solution 14 solution)