(ns aoc.day18
  (:require [aoc.utils :as u])
  (:import java.util.BitSet))

(defn- parse [input]
  (->> input
       (re-seq #"\d+")
       (map parse-long)
       (partition 3)
       (map vec)))

(defn- extent [xs]
  (and (seq xs)
       (reduce (fn [[min-x max-x] x] [(min min-x x) (max max-x x)])
               [Long/MAX_VALUE Long/MIN_VALUE]
               xs)))

#_(def around [[0 0 1] [0 0 -1] [0 1 0] [0 -1 0] [1 0 0] [-1 0 0]])

#_(defn- surface-area [cubes]
    (apply + (for [c cubes]
               (count (filter #(->> % (u/+p c) (contains? cubes) not) around)))))

(defn- layers
  "Returns a sequence of bitsets representing each layer in dimension dim.
  vdim and hdim must be the indices of the remaining dimensions and are used to
  encode the 2D surface into the bitset."
  [cubes dim vdim hdim]
  (let [[h-min h-max] (extent (map #(% hdim) cubes))
        width (inc (- h-max h-min))]
    (for [[_ layer] (sort-by key (group-by #(% dim) cubes))]
      (let [bitset (BitSet.)]
        (doseq [cube layer]
          (.set bitset (+ (cube hdim)
                          (* width (inc (- (cube vdim) h-min))))))
        bitset))))

(defn- layer-faces
  "Returns the number of faces along each layer along dimension dim."
  [cubes dim vdim hdim]
  (loop [last (BitSet.)
         layers (vec (layers cubes dim vdim hdim))
         result []]
    (if-let [x (peek layers)]
      (recur x (pop layers) (conj result (do (.xor last x) (.cardinality last))))
      (conj result (.cardinality last)))))

(defn- solution [input]
  (let [cubes (parse input)]
    [(+ (apply + (layer-faces cubes 0 1 2))
        (apply + (layer-faces cubes 1 2 0))
        (apply + (layer-faces cubes 2 0 1)))]))

(u/add-solution 18 solution)