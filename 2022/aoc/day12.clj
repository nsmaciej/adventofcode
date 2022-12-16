(ns aoc.day12
  (:require [aoc.utils :as u]))

(defn- find-distance [chart starts end]
  (loop [unseen (u/queue starts)
         dists (zipmap starts (repeat 0))]
    (let [p (peek unseen)
          height (int (chart p))
          front (filter #(and (not (contains? dists %)) (<= (- (int (chart %)) height) 1))
                        (u/points chart u/around p))
          front-dists (map #(vector % (inc (dists p))) front)]
      (if (= p end)
        (dists p)
        (recur (reduce conj (pop unseen) front)
               (into dists front-dists))))))

(defn- chart-find [chart value]
  (keep #(when (= (val %) value) (key %)) chart))

(defn- solution [data]
  (let [chart (u/parse-chart data)
        start (first (chart-find chart \S))
        end (first (chart-find chart \E))
        chart* (assoc chart start \a, end \z)]
    (pcalls #(find-distance chart* [start] end)
            #(find-distance chart* (chart-find chart* \a) end))))

(u/add-solution 12 solution)