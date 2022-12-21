(ns aoc.day20
  (:require [aoc.utils :as u])
  (:import java.util.LinkedList))

(defn- mix-indices! [data ^LinkedList ixs ix]
  (let [shift (nth data ix)
        from (.indexOf ixs ix)
        to (mod (+ ix shift) (dec (count data)))]
    (doto ixs
      (.remove (int from))
      (.add to ix))))

(defn- mix [data times]
  (let [n (count data)
        ixs (LinkedList. (range n))]
    (dotimes [ix (* n times)]
      (mix-indices! data ixs (mod ix n)))
    (let [mixed (mapv #(nth data %) ixs)
          zero-ix (.indexOf mixed 0)]
      (apply + (map #(nth mixed (mod (+ zero-ix %) n))
                    [1000 2000 3000])))))

(defn- solution [input]
  (let [data (mapv parse-long (re-seq #"-?\d+" input))]
    [(mix data 1)
     (mix (mapv #(* % 811589153) data) 10)]))

(u/add-solution 20 solution)