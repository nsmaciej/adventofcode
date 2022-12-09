(ns aoc.day09
  (:require [clojure.core.match :refer [match]]
            [aoc.utils :as u]))

(defn- knot-move
  "Given a head and a tail position, return the move the tail must make."
  [[hy hx] [ty tx]]
  (let [dy (- hy ty)
        dx (- hx tx)
        sy (if (< dy 0) -1 1)
        sx (if (< dx 0) -1 1)]
    (match [dy dx]
      [(:or -1 0 1) (:or -1 0 1)] [0 0]
      [0 _] [0 sx]
      [_ 0] [sy 0]
      :else [sy sx])))

(defn- parse
  "Parse the text input, returning a sequence of repeated moves."
  [input]
  (->> input
       (re-seq #"\w+")
       (partition 2)
       (mapcat (fn [[d n]] (repeat (parse-long n) (keyword d))))))

(def move->delta {:R [0 1] :L [0 -1] :U [-1 0] :D [1 0]})

(defn move
  "Given a chain, head first, applies a move and returns a new chain."
  [[head & tail] move]
  (reductions (fn [parent knot]
                (u/+p knot (knot-move parent knot)))
              (u/+p head (move->delta move))
              tail))

(defn- solve-for-size [k data]
  (->> data
       (reductions move (repeat k [0 0]))
       (map last) ; Get only tail positions
       set
       count))

(defn solution [input]
  (let [data (parse input)]
    [(solve-for-size 2 data)
     (solve-for-size 10 data)]))