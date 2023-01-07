(ns aoc.day17
  (:require [aoc.utils :as u]
            [clojure.string :as str]))

(defrecord Point [y x])
(defn- +p [p1 p2] (->Point (+ (:y p1) (:y p2)) (+ (:x p1) (:x p2))))

(def ^:private start-offset (->Point 3 2))
(def ^:private field-width 7)

;; Written bottom-to-top right-to-left. We think ltr but pack bits rtl.
(def ^:private shapes
  [{:rows [2r1111] :width 4}
   {:rows [2r010 2r111 2r010] :width 3}
   {:rows [2r111 2r100 2r100] :width 3}
   {:rows [2r1 2r1 2r1 2r1] :width 1}
   {:rows [2r11 2r11] :width 2}])

(defn- collides? [field shape {:keys [y x]}]
  (or (neg? y) (neg? x) (> (+ x (:width shape)) field-width) ; Walls.
      (and (< y (count field)) ; Fast path: the shape is above the y-start line.
           (->> (:rows shape)
                (map-indexed #(bit-and (bit-shift-left %2 x) (nth field (+ y %1) 0)))
                (some pos?)))))

(defn- glue-shape [field rows {:keys [y x]}]
  (let [grow-by (- (+ y (count rows)) (count field))
        field (if (pos? grow-by) (into field (repeat grow-by 0)) field)]
    (reduce #(update %1 (+ y %2) bit-or (bit-shift-left (rows %2) x))
            field
            (range (count rows)))))

(defn- drop-shape [nudges {:keys [field nudge-ix step]} shape-ix]
  (let [shape-ix (mod shape-ix (count shapes))
        shape (nth shapes shape-ix)]
    (loop [nudge-ix nudge-ix
           pos-start (+p (->Point (count field) 0) start-offset)]
      (let [nudge-ix (mod nudge-ix (count nudges))
            pos-nudged (+p pos-start (nth nudges nudge-ix))
            pos (if (collides? field shape pos-nudged) pos-start pos-nudged)
            pos-fallen (+p pos (->Point -1 0))]
        (if (collides? field shape pos-fallen)
          {:field (glue-shape field (:rows shape) pos)
           :nudge-ix (inc nudge-ix)
           :shape-ix (inc shape-ix)
           :step (inc step)}
          (recur (inc nudge-ix) pos-fallen))))))

(defn- field-key [field]
  (let [rows (subvec field (max 0 (- (count field) (quot 64 field-width))))]
    (transduce (map-indexed #(bit-shift-left %2 (* %1 field-width)))
               (completing bit-or) 0 rows)))

(defn- find-cycle [seen state]
  (let [key [(:nudge-ix state) (:shape-ix state) (field-key (:field state))]]
    (if-let [state* (get seen key)] (reduced [state* state]) (assoc seen key state))))

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
  (let [nudges (mapv {\> (->Point 0 1), \< (->Point 0 -1)} (str/trim input))
        start {:field (vector-of :long), :nudge-ix 0, :step 0}
        states (reductions #(drop-shape nudges %1 %2) start (range))]
    [(-> states (nth 2022) :field count), (part-2 nudges states)]))

(u/add-solution 17 solution)