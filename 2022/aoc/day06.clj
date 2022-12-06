(ns aoc.day06)

(defn- find-marker [input n]
  (->> input
       (partition n 1)
       (map set)
       (map-indexed vector)
       (filter #(= (count (second %)) n))
       first
       first
       (+ n)))

(defn solution [input]
  [(find-marker input 4)
   (find-marker input 14)])