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

(defn- glue-shape [field rows [y x]]
  (let [grow-by (- (+ y (count rows)) (count field))
        field (if (pos? grow-by) (into field (repeat grow-by 0)) field)]
    (reduce #(update %1 (+ y %2) bit-or (bit-shift-left (rows %2) x))
            field
            (range (count rows)))))

(defn- print-field [field]
  (println "---------")
  (doseq [row (reverse field)]
    (println (format "|%-7s|" (str/reverse (Long/toString row 2)))))
  (println "---------"))

(defn- drop-shape [nudges {:keys [field nudge-ix]} shape-ix]
  (let [shape-ix (mod shape-ix (count shapes))
        shape (nth shapes shape-ix)]
    (loop [nudge-ix nudge-ix
           pos-start (u/+p [(count field) 0] start-offset)]
      (let [nudge-ix (mod nudge-ix (count nudges))
            nudge (nth nudges nudge-ix)
            pos-nudged (u/+p pos-start nudge)
            pos (if (collides? field shape pos-nudged) pos-start pos-nudged)
            pos-fallen (u/-p pos [1 0])]
        (if (collides? field shape pos-fallen)
          (array-map :field (glue-shape field (:rows shape) pos)
                     :nudge-ix (inc nudge-ix)
                     :shape-ix (inc shape-ix))
          (recur (inc nudge-ix) pos-fallen))))))

(defn- step-key [step]
  [(:nudge-ix step)
   (:shape step)
   (let [field (:field step), rows (quot 64 field-width)]
     (reduce bit-or (map-indexed #(bit-shift-left %2 (* %1 field-width))
                                 (subvec field (max 0 (- (count field) rows))))))])

(defn- find-cycle [steps]
  (reduce (fn [seen [i step]]
            (let [key (step-key step)
                  step (assoc step :step i)]
              (if-let [matching-step (get seen key)]
                (reduced [matching-step step])
                (assoc seen key step))))
          {}
          (map-indexed vector steps)))

(defn- part-1 [nudges steps]
  (let [start (array-map :field (vector-of :int), :nudge-ix 0)]
    (reduce #(drop-shape nudges %1 %2) start (range steps))))

(defn- part-2 [nudges steps]
  (let [real-start (array-map :field (vector-of :int), :nudge-ix 0)
        step-states (reductions #(drop-shape nudges %1 %2) real-start (range))
        [start end] (find-cycle (next step-states))
        height-delta (- (-> end :field count) (-> start :field count))
        step-delta (- (:step end) (:step start))
        cycle-count (quot (- steps (:step start)) step-delta)
        steps-left (- steps (* cycle-count step-delta) (:step start))
        height (+ (* height-delta cycle-count) (-> start :field count))]
    (+ height
       (- (count (:field (reduce #(drop-shape nudges %1 %2) start (range (:shape-ix start) (+ steps-left (:shape-ix start))))))
          (count (:field start))
          1))))

(defn- solution [input]
  (let [nudges (mapv {\> [0 1] \< [0 -1]} (str/trim input))]
    [(count (:field (part-1 nudges 2022)))
     (part-2 nudges 1000000000000)]))

(u/add-solution 17 solution)