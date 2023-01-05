(ns aoc.utils
  (:require [clojure.string :as str]
            [aoc.utils :as u]))

(defn input
  "Return the input for day n."
  [n]
  (slurp (format "inputs/day%02d.txt" n)))

(defonce solutions (atom (sorted-map)))

(defmacro add-solution
  "Register a solution function for a given day. Automatically uses an interned
  var so that solution functions need not be re-added whenever they change."
  [day solution]
  `(swap! solutions assoc ~day #'~solution))

;; Useful for quick testing.
(defn solve
  "Solve day n with standard input"
  ([n] (vec ((@solutions n) (input n))))
  ([n text] (vec ((@solutions n) text))))

(defn queue
  ([] (clojure.lang.PersistentQueue/EMPTY))
  ([coll]
   (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

(defn maximum
  ([coll] (maximum identity coll))
  ([xf coll]
   (if (empty? coll) nil (transduce xf (completing max) Long/MIN_VALUE coll))))

(defn minimum
  ([coll] (minimum identity coll))
  ([xf coll]
   (if (empty? coll) nil (transduce xf (completing min) Long/MAX_VALUE coll))))

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
  "Add points together"
  ([] [0 0])
  ([p] p)
  ([p1 p2] (mapv + p1 p2))
  ([p1 p2 p3] (mapv + p1 p2 p3))
  ([p1 p2 p3 & more] (apply mapv + p1 p2 p3 more)))

(defn -p
  "Subtract points from each other"
  ([p] p)
  ([p1 p2] (mapv - p1 p2))
  ([p1 p2 p3] (mapv - p1 p2 p3))
  ([p1 p2 p3 & more] (apply mapv - p1 p2 p3 more)))

(defn abs-
  "Return the abosulte difference of two numbers."
  [x y]
  (abs (- x y)))

(defn manhattan
  "Find the manhattan distance between two points."
  [[y1 x1] [y2 x2]]
  (+ (u/abs- y1 y2) (u/abs- x1 x2)))

(def around
  "All the points around a the point [0 0]."
  [[-1 0] [0 -1] [0  1] [1  0]])

(def around*
  "All the points around a the point [0 0], including diagonals."
  [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]])

(defn keys*
  "Returns a sequence of keys, but also treats non-maps as if they were keys."
  [coll]
  (if (map? coll)
    (keys coll)
    (seq coll)))

(defn update-keys*
  "Like update-keys, but also maps over non-maps."
  [coll f]
  (if (map? coll)
    (update-keys coll f)
    (into (empty coll) (map f) coll)))

(defn chart+p [chart & ps]
  (update-keys* chart #(apply +p % ps)))

(defn chart-p [chart & ps]
  (update-keys* chart #(apply -p % ps)))

(defn chart-extent [chart]
  (let [[min-y max-y min-x max-x]
        (if (empty? chart)
          [0 0 0 0]
          (reduce (fn [[min-y max-y min-x max-x] [y x]]
                    [(min min-y y) (max max-y y) (min min-x x) (max max-x x)])
                  [Long/MAX_VALUE Long/MIN_VALUE Long/MAX_VALUE Long/MIN_VALUE]
                  (keys* chart)))]
    {:min-y min-y, :max-y (inc max-y), :min-x min-x, :max-x (inc max-x)
     :height (inc (- max-y min-y))
     :width (inc (- max-x min-x))}))

(def ^:dynamic *present-grid-value*
  "Value to use when turning set-based charts into grids"
  \#)

(defn chart->grid
  "Convert a chart into a lazy grid, replacing missing elements with `blank` or
  nil if not provided. Note the resulting grid may be very large for very sparse
  charts."
  ([chart]
   (chart->grid nil chart))
  ([blank chart]
   (let [extent (chart-extent chart)
         x-range (range (:min-x extent) (:max-x extent))
         y-range (range (:min-y extent) (:max-y extent))]
     (for [y y-range]
       (for [x x-range :let [p [y x]]]
         (if (map? chart)
           (get chart p blank)
           (if (contains? chart p) *present-grid-value* blank)))))))

(defn points
  "Returns `points` which belong to the chart, optionally adding
   point `p` before checking."
  ([chart deltas p]
   (eduction (map #(+p p %))
             (filter #(contains? chart %))
             deltas))
  ([chart points]
   (filter #(contains? chart %)
           points)))

(defn values
  "Behaves like `points` but returns the value associated with the points."
  ([chart deltas p]
   (eduction (map #(+p p %))
             (filter #(contains? chart %))
             (map #(get chart %))
             deltas))
  ([chart points]
   (eduction (filter #(contains? chart %))
             (map #(get chart %))
             points)))

(defn- grid-vstack [& grids]
  (vec (apply concat grids)))

(defn- grid-hstack [& grids]
  (apply mapv #(vec (apply concat %&)) grids))

(defn- border-grid [grid]
  (let [w (count (first grid))
        h (count grid)
        vertical (concat [[\+]] (repeat h [\|]) [[\+]])]
    (grid-hstack vertical
                 (grid-vstack [(repeat w \-)] grid [(repeat w \-)])
                 vertical)))

(defn chart-str
  "Convert a chart to a multi-line string, replacing missing elements with
  `blank` or a space if not provided."
  [chart & {:keys [blank border] :or {blank \., border true}}]
  (let [grid (chart->grid blank chart)]
    (->> (if border (border-grid grid) grid)
         (map str/join)
         (str/join "\n"))))

(defmethod print-method ::chart [chart w]
  (.write w (chart-str chart)))

(defn parse-chart [s]
  (let [lines (str/split-lines s)]
    (with-meta
      (into {} (for [y (range (count lines))
                     x (range (count (nth lines y)))]
                 [[y x] (nth (nth lines y) x)]))
      {:type ::chart})))

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
  ([x y & more] (reduce lcm (lcm x y) more)))

;; Just like real update we overload like crazy. Seems like apply is
;; just very slow and if we do not do this, day 11 is 2x slower.
(defn update!
  "Update a transient associative structure"
  ([m k f] (assoc! m k (f (get m k))))
  ([m k f a] (assoc! m k (f (get m k) a)))
  ([m k f a b] (assoc! m k (f (get m k) a b)))
  ([m k f a b c] (assoc! m k (f (get m k) a b c)))
  ([m k f a b c d] (assoc! m k (f (get m k) a b c d)))
  ([m k f a b c d & args] (assoc! m k (apply f (nth m k) a b c d args))))

(defn sliding-pair
  "A transducer that returns a two-element sliding window of its input.
  Same as (partition 2 1) might do if it returned a transducer."
  [xf]
  (let [prev (volatile! ::none)]
    (fn
      ([] (xf))
      ([result] (xf result))
      ([result input]
       (let [prior @prev]
         (vreset! prev input)
         (if (= prior ::none)
           result
           (xf result [prior input])))))))