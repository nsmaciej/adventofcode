(ns aoc.day08
  (:require [clojure.string :as str]
            [aoc.utils :as u]))

(defn- visible [xs]
  (->> xs
       (cons -1)
       (reductions max)
       (map > xs)))

(defn- visible-both [xs]
  (map #(or %1 %2)
       (-> xs reverse visible reverse)
       (visible xs)))

(defn- parse [input]
  (mapv (fn [line] (mapv #(- (int %) 48) line))
        (str/split-lines input)))

(defn- part1 [data]
  (let [vismap (u/map-grid #(or %1 %2)
                           (map visible-both data)
                           (->> data
                                u/transpose-grid
                                (map visible-both)
                                u/transpose-grid))]
    (count (u/filter-grid identity vismap))))

(defn solution [input]
  (let [data (parse input)]
    [(part1 data) nil]))

(comment
  (let [xs [3 0 3 7 3]]
    (reductions max (cons 0 xs)))
  ;; => (0 3 3 3 7 7)


  (let [xs [3 0 3 7 3]]
    (map > xs (reductions max (cons 0 xs))))
  ;; => (true false false true false)
  )