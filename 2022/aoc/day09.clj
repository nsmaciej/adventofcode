(ns aoc.day09
  (:require [aoc.utils :as u]))

(def move->delta
  {:R [0 1], :L [0 -1], :U [-1 0], :D [1 0]})

(defn- parse
  "Returns a list of keywords. Repeated movements give repeated values"
  [input]
  (->> input
       (re-seq #"\w+")
       (partition 2)
       (mapcat (fn [[d n]] (repeat (parse-long n) (keyword d))))))

(defn- follow
  "Given a head and a tail position, return the move the tail must make."
  [[hy hx] [ty tx]]
  (let [dy (- hy ty)
        dx (- hx tx)]
    (if (< (max (abs dx) (abs dy)) 2)
      [0 0]
      [(u/sign dy) (u/sign dx)])))

(defn move
  "Given a chain, head first, applies a move and returns a new chain."
  [[head & tail] move]
  (reductions #(u/+p %2 (follow %1 %2))
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