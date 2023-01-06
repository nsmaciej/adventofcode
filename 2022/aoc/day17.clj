(ns aoc.day17
  (:require [aoc.utils :as u]
            [clojure.string :as str]))

(def ^:private start-offset [3 2])
(def ^:private field-width 7)

;; Written bottom-to-top right-to-left. We think ltr but pack bits rtl.
(def shapes
  [{:rows [2r1111] :width 4}
   {:rows [2r010 2r111 2r010] :width 3}
   {:rows [2r111 2r100 2r100] :width 3}
   {:rows [2r1 2r1 2r1 2r1] :width 1}
   {:rows [2r11 2r11] :width 2}])

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
          {:field (glue-shape field (:rows shape) pos)
           :nudge-ix (inc nudge-ix)
           :shape-ix (inc shape-ix)}
          (recur (inc nudge-ix) pos-fallen))))))

(defn- step-key [step]
  (let [field (:field step)
        rows (subvec field (max 0 (- (count field) (quot 64 field-width))))]
    [(:nudge-ix step)
     (:shape step)
     (reduce bit-or 0 (map-indexed #(bit-shift-left %2 (* %1 field-width)) rows))]))

(defn find-cycle [seen state]
  (let [key (step-key state), step (assoc state :step (count seen))]
    (if-let [matching-step (get seen key)]
      (reduced [matching-step step])
      (assoc seen key step))))

(defn- part-2 [nudges states]
  (let [part-2-steps 1000000000000
        [start end] (reduce find-cycle {} states)
        start-step (:step start)
        cycle-growth (- (count (:field end)) (count (:field start)))
        cycle-steps (- (:step end) start-step)
        cycles (quot (- part-2-steps start-step) cycle-steps)
        steps-left (- part-2-steps (* cycles cycle-steps) start-step)]
    ;; Simulate the final steps-left steps and add the cycles we "cut".
    (->> (range steps-left)
         (reduce #(drop-shape nudges %1 (+ (:shape-ix start) %2)) start)
         :field
         count
         (+ (* cycle-growth cycles)))))

(defn- solution [input]
  (let [nudges (mapv {\> [0 1] \< [0 -1]} (str/trim input))
        start {:field (vector-of :long), :nudge-ix 0}
        states (reductions #(drop-shape nudges %1 %2) start (range))]
    [(-> states (nth 2022) :field count), (part-2 nudges states)]))

(u/add-solution 17 solution)