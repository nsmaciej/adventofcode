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

(defn register
  "Register a solution function for the given day."
  [day solution]
  (swap! solutions assoc day solution))