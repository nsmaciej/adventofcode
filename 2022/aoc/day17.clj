(ns aoc.day17
  (:require [aoc.utils :as u]
            [clojure.string :as str]))

(def ^:private shapes
  [[[0 0] [0  1] [0  2] [0  3]]        ; rope 
   [[0 1] [-1 0] [-1 1] [-1 2] [-2 1]] ; cross 
   [[0 0] [0  1] [0  2] [-1 2] [-2 2]] ; bracket 
   [[0 0] [-1 0] [-2 0] [-3 0]]        ; stick 
   [[0 0] [0  1] [-1 0] [-1 1]]])      ; block 

(def ^:private start-offset [-3 2])
(def ^:private field-width 7)

(defn- collides? [field shape]
  (or (> (apply max (map first shape)) 0)
      (< (apply min (map second shape)) 0)
      (>= (apply max (map second shape)) field-width)
      (some #(contains? field %) shape)))

(defn drop-shape
  "Simulate a single shape dropping. `field` should be a set, `stream` an infite
  sequence of moves to nudge the shape by, `shape` a seq of points with
  their origin at [0 0], `y-start` the top the field."
  [shape field stream y-start]
  (loop [[nudge & stream] stream
         shape (u/chart+p shape [(dec y-start) 0] start-offset)]
    (let [shape-nudged (u/chart+p shape nudge)
          shape* (if (collides? field shape-nudged) shape shape-nudged)
          shape-pulled (u/chart+p shape* [1 0])]
      (if (collides? field shape-pulled)
        [(into field shape*), stream, (apply min y-start (map first shape*))]
        (recur stream shape-pulled)))))

(defn- part-1 [moves steps]
  (->> (take steps (cycle (vals shapes)))
       ;; If the floor is at y=0, then last block would've been at y=1.
       (reduce #(apply drop-shape %2 %1) [#{} (cycle moves) 1])
       first
       (map first)
       (apply min)
       -
       inc))

(defn- solution [input]
  (let [moves (map {\> [0 1] \< [0 -1]} (str/trim input))]
    [(part-1 moves 2022)]))

(u/add-solution 17 solution)