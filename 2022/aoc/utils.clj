(ns aoc.utils)

(defn- input-file [n]
  (format "inputs/day%02d.txt" n))

(defn input [n]
  (slurp (input-file n)))

(defn transpose-grid
  "Transose a grid"
  [grid]
  (apply mapv vector grid))

(defn map-grid
  "Map through multiple grids in a zip-like manner."
  [f & grids]
  (apply map (fn [& rows] (apply map f rows)) grids))

(defn filter-grid
  "Filters through the grid, returning a flattened result."
  [f grid]
  (mapcat #(filter f %) grid))