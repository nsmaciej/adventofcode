(ns aoc.day08
  (:require [clojure.string :as str]
            [aoc.utils :as u]))

(defn- visible [xs]
  (map > xs (reductions max -1 xs)))

(defn- visible-both [xs]
  (map #(or %1 %2) (-> xs reverse visible reverse) (visible xs)))

(defn- parse [input]
  (u/mapv-grid #(- (int %) 48) (str/split-lines input)))

(defn- part1 [data]
  (let [left-right (map visible-both data)
        top-down (->> data
                      u/transpose-grid
                      (map visible-both)
                      u/transpose-grid)]
    (->> (u/map-grid #(or %1 %2) top-down left-right)
         (u/filter-grid identity)
         count)))

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