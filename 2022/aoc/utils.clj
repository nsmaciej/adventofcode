(ns aoc.utils
  (:require [clojure.string :as str]
            [aoc.utils :as u]
            [criterium.core :as criterium]))

(defn input-file
  "Get the path the input file for day n."
  [n]
  (format "inputs/day%02d.txt" n))

(defn input
  "Return the input for day n."
  [n]
  (slurp (input-file n)))

(defn queue
  ([] (clojure.lang.PersistentQueue/EMPTY))
  ([coll]
   (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

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

(defn chart-extent [chart]
  (zipmap [:min-y :max-y :min-x :max-x]
          (if (empty? chart)
            [0 0 0 0]
            (reduce (fn [[min-y max-y min-x max-x] [y x]]
                      [(min min-y y) (max max-y y) (min min-x x) (max max-x x)])
                    [Long/MAX_VALUE Long/MIN_VALUE Long/MAX_VALUE Long/MIN_VALUE]
                    (keys chart)))))

(defn chart->grid
  "Convert a chart into a lazy grid, replacing missing elements with `blank` or
  nil if not provided. Note the resulting grid may be very large for very sparse
  charts."
  ([chart]
   (chart->grid nil chart))
  ([blank chart]
   (let [extent (chart-extent chart)
         x-range (range (:min-x extent) (inc (:max-x extent)))
         y-range (range (:min-y extent) (inc (:max-y extent)))]
     (map (fn [y]
            (map #(get chart [y %] blank) x-range))
          y-range))))

(defn aroundp*
  "Returns all points around a point."
  [p]
  [(+p p [-1 0]) (+p p [0 -1]) (+p p [0  1]) (+p p [1  0])])

(defn aroundp
  "Returns non-empty points around a point."
  [chart p]
  (filter #(contains? chart %) (aroundp* p)))

(defn chart-str
  "Convert a chart to a multi-line string, replacing missing elements with
  `blank` or a space if not provided."
  ([chart]
   (chart-str \space chart))
  ([blank chart]
   (->> chart
        (chart->grid blank)
        (map str/join)
        (str/join "\n"))))

(defn parse-chart [s]
  (let [lines (str/split-lines s)]
    (into {}
          (for [y (range (count lines))
                x (range (count (nth lines y)))]
            [[y x] (nth (nth lines y) x)]))))

(defn sign
  "Returns -1, 0, or 1 depending on the sing of x."
  [x]
  (cond (< x 0) -1
        (> x 0) 1
        :else 0))

(defn gcd [a b]
  (if (zero? b) a (recur b (mod a b))))

(defn lcm
  "Returns the lowest common multiple of given numbers."
  ([x] x)
  ([x y] (/ (* x y) (gcd x y)))
  ([x y & rest] (reduce lcm (lcm x y) rest)))

;; TODO: Just like real update we overload like crazy. Why does it do
;; it that way. If we do not do this, day 11 is 2x slower.
(defn update!
  "Update a transient associative structure"
  ([m k f] (assoc! m k (f (get m k))))
  ([m k f a] (assoc! m k (f (get m k) a)))
  ([m k f a b] (assoc! m k (f (get m k) a b)))
  ([m k f a b c] (assoc! m k (f (get m k) a b c)))
  ([m k f a b c d] (assoc! m k (f (get m k) a b c d)))
  ([m k f a b c d & args] (assoc! m k (apply f (nth m k) a b c d args))))

(def solutions (atom {}))

(defn add-solution
  "Register a solution function for the given day."
  [day solution]
  (swap! solutions assoc day solution))