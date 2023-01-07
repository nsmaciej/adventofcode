(ns aoc.day17
  (:require [aoc.utils :as u]
            [clojure.string :as str]))

(defrecord Point [y x])
(defn- +p [p1 p2] (->Point (+ (:y p1) (:y p2)) (+ (:x p1) (:x p2))))

(def ^:private start-offset (->Point 3 2))
(def ^:private field-width 7)
(def ^:private cycle-check-rows 6)

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
                (map-indexed #(bit-and (bit-shift-left %2 x)
                                       (nth field (+ y %1) 0)))
                (some pos?)))))

(defn- glue-shape [field rows {:keys [y x]}]
  ;; Update one past the end gives us a nil.
  (reduce (fn [f r]
            (update f (+ y r) #(bit-or (or % 0) (bit-shift-left (rows r) x))))
          field
          (range (count rows))))

(defn- drop-shape [nudges {:keys [field nudge-ix]} step]
  (let [shape-ix (mod step (count shapes))
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

(defn- find-cycle [seen state]
  (let [ix (max 0 (- (count (:field state)) cycle-check-rows))
        key [(:nudge-ix state) (:shape-ix state) (subvec (:field state) ix)]]
    (if-let [state* (get seen key)]
      (reduced [state* state])
      (assoc seen key state))))

(defn- part-2 [states]
  (let [part-2-steps 1000000000000
        [start end] (reduce find-cycle {} states)
        start-step (:step start)
        cycle-growth (- (count (:field end)) (count (:field start)))
        cycle-steps (- (:step end) start-step)
        cycles (quot (- part-2-steps start-step) cycle-steps)
        steps-left (- part-2-steps (* cycles cycle-steps) start-step)]
    (+ (-> states (nth (+ start-step steps-left)) :field count)
       (* cycle-growth cycles))))

(defn- solution [input]
  (let [nudges (mapv {\> (->Point 0 1), \< (->Point 0 -1)} (str/trim input))
        start {:field (vector-of :long), :nudge-ix 0, :step 0}
        states (reductions #(drop-shape nudges %1 %2) start (range))]
    [(-> states (nth 2022) :field count), (part-2 states)]))

(u/add-solution 17 solution)