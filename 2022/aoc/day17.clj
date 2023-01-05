(ns aoc.day17
  (:require [aoc.utils :as u]
            [clojure.string :as str]))

(def ^:private shapes
  [[[0 0] [0 1] [0 2] [0 3]]       ; rope 
   [[0 1] [1 0] [1 1] [1 2] [2 1]] ; cross 
   [[0 0] [0 1] [0 2] [1 2] [2 2]] ; bracket 
   [[0 0] [2 0] [1 0] [3 0]]       ; stick 
   [[0 0] [0 1] [1 0] [1 1]]])     ; block 

(def ^:private start-offset [3 2])
(def ^:private field-width 7)

(defn- collides? [field shape]
  (or (< (u/minimum (map first) shape) 0)
      (< (u/minimum (map second) shape) 0)
      (>= (u/maximum (map second) shape) field-width)
      (some #(contains? field %) shape)))

(defn drop-shape
  "Simulate a single shape dropping. `field` should be a set, `nudges` an infite
   sequence of moves, `shape` a point seq with origin at [0 0], `y-start` top the field."
  [{:keys [field, nudges, y-start]} shape]
  (loop [[nudge & nudges] nudges
         shape (u/chart+p shape [y-start 0] start-offset)]
    (let [shape-nudged (u/chart+p shape nudge)
          shape* (if (collides? field shape-nudged) shape shape-nudged)
          shape-pulled (u/chart-p shape* [1 0])]
      (if (collides? field shape-pulled)
        (array-map :field (into field shape*)
                   :nudges nudges
                   :y-start (max y-start (inc (u/maximum (map first) shape*))))
        (recur nudges shape-pulled)))))

(defn- part-1 [moves steps]
  (let [future-shapes (take steps (cycle shapes))
        start (array-map :field #{}, :nudges (cycle moves), :y-start 0)]
    (:y-start (reduce drop-shape start future-shapes))))

(defn- solution [input]
  (let [moves (map {\> [0 1] \< [0 -1]} (str/trim input))]
    [(part-1 moves 2022)]))

(u/add-solution 17 solution)