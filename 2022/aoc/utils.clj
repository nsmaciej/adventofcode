(ns aoc.utils
  (:require [clojure.string :as str]))

(defn input-file
  "Get the path the input file for day n."
  [n]
  (format "inputs/day%02d.txt" n))

(defn input
  "Return the input for day n."
  [n]
  (slurp (input-file n)))

(defn transpose-grid
  "Transose a grid"
  [grid]
  (apply map list grid))

(defn transposev-grid
  "Transose a grid. Returning a vector of vectors."
  [grid]
  (apply mapv vector grid))

(defn map-grid
  "Map through multiple grids in a zip-like manner."
  [f & grids]
  (apply map (fn [& rows] (apply map f rows)) grids))

(defn mapv-grid
  "Map through multiple grids in a zip-like manner.
   Returning a vector of vectors."
  [f & grids]
  (apply mapv (fn [& rows] (apply mapv f rows)) grids))

(defn filter-grid
  "Filters through the grid, returning a flattened result."
  [f grid]
  (mapcat #(filter f %) grid))