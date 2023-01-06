(ns aoc.day17
  (:require [aoc.utils :as u]
            [clojure.string :as str]))

(def ^:private start-offset [3 2])
(def ^:private field-width 7)

(defn- encode [[y x]]
  (+ (* y field-width) x))

(def ^:private shapes
  (mapv #(array-map :points (mapv encode %)
                    :width (u/maximum (map second) %)
                    :height (inc (u/maximum (map first) %)))
        [[[0 0] [0 1] [0 2] [0 3]]       ; rope
         [[0 1] [1 0] [1 1] [1 2] [2 1]] ; cross
         [[0 0] [0 1] [0 2] [1 2] [2 2]] ; bracket
         [[0 0] [2 0] [1 0] [3 0]]       ; stick
         [[0 0] [0 1] [1 0] [1 1]]]))    ; block

(defn- collides? [field shape y-start [y x]]
  (let [{:keys [width points]} (shapes shape)
        ix (encode [y x])]
    (or (< y 0) (< x 0) (>= (+ x width) field-width)
        (and (<= y y-start) ; Fast path.
             (some #(contains? field (+ % ix)) points)))))

(defn drop-shape
  "Simulate a single shape dropping. `field` should be a set, `nudges` an infite
   sequence of moves, `shape` a point seq with origin at [0 0], `y-start` top the field."
  [{:keys [field nudges y-start]} shape]
  (let [{:keys [height points]} (shapes shape)]
    (loop [[nudge & nudges] nudges
           pos (u/+p [y-start 0] start-offset)]
      (let [pos-nudged (u/+p pos nudge)
            pos* (if (collides? field shape y-start pos-nudged) pos pos-nudged)
            pos*-ix (encode pos*)
            pos-fallen (u/-p pos* [1 0])]
        (if (collides? field shape y-start pos-fallen)
          (array-map :field (into field (map #(+ % pos*-ix)) points)
                     :nudges nudges
                     :y-start (max y-start (+ (first pos) height)))
          (recur nudges pos-fallen))))))

(defn- part-1 [moves steps]
  (let [future-shapes (->> shapes count range cycle (take steps))
        start (array-map :field #{}, :nudges (cycle moves), :y-start 0)]
    (:y-start (reduce drop-shape start future-shapes))))

(defn- solution [input]
  (let [moves (map {\> [0 1] \< [0 -1]} (str/trim input))]
    [(part-1 moves 2022)]))

(u/add-solution 17 solution)