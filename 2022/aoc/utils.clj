(ns aoc.utils)

(defn input-file
  "Get the path the input file for day n."
  [n]
  (format "inputs/day%02d.txt" n))

(defn input
  "Return the input for day n."
  [n]
  (slurp (input-file n)))

(defn transpose
  "Transose a grid"
  [grid]
  (apply map list grid))

(defn map-grid
  "Map through multiple grids in a zip-like manner."
  [f & grids]
  (apply map (fn [& rows] (apply map f rows)) grids))

(defn mapv-grid
  "Map through multiple grids in a zip-like manner.
   Returning a vector of vectors."
  [f & grids]
  (apply mapv (fn [& rows] (apply mapv f rows)) grids))

(defn +p
  "Add  points together"
  ([] [0 0])
  ([[y x]] [y x])
  ([[y1 x1] [y2 x2]] [(+ y1 y2) (+ x1 x2)])
  ([p1 p2 & rest] (reduce +p (+p p1 p2) rest)))