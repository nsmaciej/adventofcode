(ns aoc.day17
  (:require [aoc.utils :as u]
            [clojure.string :as str]))

(def ^:private start-offset [3 2])
(def ^:private field-width 7)

;; Written bottom-to-top right-to-left - from hereon we think left-to-right but pack
;; bits right-to-left.
(def shapes
  [{:rows [2r1111] :width 4 :name :snake}
   {:rows [2r010 2r111 2r010] :width 3 :name :cross}
   {:rows [2r111 2r100 2r100] :width 3 :name :bracket}
   {:rows [2r1 2r1 2r1 2r1] :width 1 :name :stick}
   {:rows [2r11 2r11] :width 2 :name :block}])

(defn- collides? [field shape [y x]]
  (or (neg? y) (neg? x) (> (+ x (:width shape)) field-width) ; Walls.
      (and (< y (count field)) ; Fast path: the shape is above the y-start line.
           (->> (:rows shape)
                (map-indexed #(bit-and (bit-shift-left %2 x) (nth field (+ y %1) 0)))
                (some pos?)))))

(defn- glue-shape
  "Glue the shape onto the field, growing it if necessary."
  [field rows [y x]]
  (let [grow-by (- (+ y (count rows)) (count field))
        field (if (pos? grow-by) (into field (repeat grow-by 0)) field)]
    (reduce #(update %1 (+ y %2) bit-or (bit-shift-left (rows %2) x))
            field
            (range (count rows)))))

(defn- drop-shape
  "Simulate a single shape dropping. `field` should be a vec, `nudge-ix` an index less
   than `(count nudges)`, `nudges` a sequence of moves, `y-start` top the field."
  [nudges {:keys [field nudge-ix]} shape]
  (loop [nudge-ix nudge-ix
         pos-start (u/+p [(count field) 0] start-offset)]
    (let [nudge (nth nudges (mod nudge-ix (count nudges)))
          pos-nudged (u/+p pos-start nudge)
          pos (if (collides? field shape pos-nudged) pos-start pos-nudged)
          pos-fallen (u/-p pos [1 0])]
      (if (collides? field shape pos-fallen)
        (array-map :field (glue-shape field (:rows shape) pos), :nudge-ix (inc nudge-ix))
        (recur (inc nudge-ix) pos-fallen)))))

(defn- part-1 [nudges steps]
  (let [future-shapes (->> shapes cycle (take steps))
        start (array-map :field (vector-of :int), :nudge-ix 0)]
    (reduce #(drop-shape nudges %1 %2) start future-shapes)))

(defn- solution [input]
  (let [nudges (map {\> [0 1] \< [0 -1]} (str/trim input))]
    [(part-1 nudges 2022)]))

(u/add-solution 17 solution)