(ns aoc.day09
  (:require [clojure.core.match :refer [match]]
            [aoc.utils :as u]
            [clojure.string :as str]))

(defn- tail-move
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

(defn move [[h t] dir]
  (let [new-h (u/+p h (move->delta dir))]
    [new-h (u/+p t (tail-move new-h t))]))

(defn- part1 [data]
  (->> data
       (reductions move [[0 0] [0 0]])
       (map second) ; Get only tail positions
       set
       count))

(defn solution [input]
  (let [data (parse input)]
    [(part1 data) nil]))

(comment
  (tail-move [0 0] [2 1])
  ;; => [-1 -1]
  (tail-move [0 2] [0 0])
  ;; => [0 1]
  )