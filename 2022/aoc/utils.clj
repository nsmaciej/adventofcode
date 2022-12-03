(ns aoc.utils
  (:require [clojure.java.io :as io]))

(defn- input-file [n]
  (format "inputs/day%02d.txt" n))

(defn input [n]
  (slurp (input-file n)))

(defn input-lines [n]
  (with-open [rdr (io/reader (input-file n))]
    (vec (line-seq rdr))))