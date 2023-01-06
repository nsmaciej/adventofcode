(ns aoc.day17
  (:require [aoc.utils :as u]
            [clojure.string :as str]))

(def ^:private start-offset [3 2])
(def ^:private field-width 7)

;; Written bottom-to-top right-to-left - from hereon we think left-to-right but pack
;; bits right-to-left.
(def shapes
  [{:rows [2r1111] :width 4}
   {:rows [2r010 2r111 2r010] :width 3}
   {:rows [2r111 2r100 2r100] :width 3}
   {:rows [2r1 2r1 2r1 2r1] :width 1}
   {:rows [2r11 2r11] :width 2}])

(defn- collides? [field shape y-start [y x]]
  (or (neg? y) (neg? x) (> (+ x (:width shape)) field-width) ; Walls.
      (and (<= y y-start) ; Fast path: the shape is above the y-start line.
           (->> (:rows shape)
                (map-indexed #(bit-and (bit-shift-left %2 x) (nth field (+ y %1) 0)))
                (some pos?)))))

(defn- glue-shape [field rows [y x]]
  (let [grow-by (- (+ y (count rows)) (count field))
        field (if (pos? grow-by) (into field (repeat grow-by 0)) field)]
    (reduce (fn [f i] (update f (+ y i) bit-or (bit-shift-left (rows i) x)))
            field
            (range (count rows)))))

(defn- print-field [field]
  (println "---------")
  (doseq [row (reverse field)]
    (println (format "|%-7s|" (str/reverse (Long/toString row 2)))))
  (println "---------"))

(defn drop-shape
  "Simulate a single shape dropping. `field` should be a set, `nudges` an infite
   sequence of moves, `shape` a point seq with origin at [0 0], `y-start` top the field."
  [{:keys [field nudges y-start]} shape]
  (loop [[nudge & nudges] nudges
         pos-start (u/+p [y-start 0] start-offset)]
    (let [pos-nudged (u/+p pos-start nudge)
          pos (if (collides? field shape y-start pos-nudged) pos-start pos-nudged)
          pos-fallen (u/-p pos [1 0])]
      (if (collides? field shape y-start pos-fallen)
        (array-map :field (glue-shape field (:rows shape) pos)
                   :nudges nudges
                   :y-start (max y-start (+ (first pos) (count (:rows shape)))))
        (recur nudges pos-fallen)))))

(defn- part-1 [moves steps]
  (let [future-shapes (->> shapes cycle (take steps))
        start (array-map :field [], :nudges (cycle moves), :y-start 0)]
    (:y-start (reduce drop-shape start future-shapes))))

(defn- solution [input]
  (let [moves (map {\> [0 1] \< [0 -1]} (str/trim input))]
    [(part-1 moves 2022)]))

(u/add-solution 17 solution)