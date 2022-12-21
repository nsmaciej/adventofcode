(ns aoc.day20
  (:require [clojure.core.rrb-vector :as fv]
            [aoc.utils :as u]))

(defn- fv-pop-at [coll i]
  (fv/catvec (fv/subvec coll 0 i)
             (fv/subvec coll (inc i))))

(defn- fv-insert-at [coll i value]
  (fv/catvec (fv/subvec coll 0 i)
             (fv/vector value)
             (fv/subvec coll i)))

(defn- fv-index-of [coll needle]
  (first (keep-indexed #(when (= %2 needle) %1) coll)))

(defn- mix-indices [data ixs ix]
  (let [shift (nth data ix)
        from (fv-index-of ixs ix)
        to (mod (+ ix shift) (dec (count data)))]
    (fv-insert-at (fv-pop-at ixs from) to ix)))

(defn- mix [data times]
  (let [n (count data)
        mixed (->> (map #(mod % n) (range (* n times)))
                   (reduce #(mix-indices data %1 %2) (fv/vec (range n)))
                   (mapv #(nth data %)))
        zero-ix (fv-index-of mixed 0)]
    (apply + (map #(nth mixed (mod (+ zero-ix %) n))
                  [1000 2000 3000]))))

(defn- solution [input]
  (let [data (mapv parse-long (re-seq #"-?\d+" input))]
    [(mix data 1)
     (mix (mapv #(* % 811589153) data) 10)]))

(u/add-solution 20 solution)