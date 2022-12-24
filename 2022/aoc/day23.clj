(ns aoc.day23
  (:require [aoc.utils :as u]))

(def ^:private first->cardinals
  {:north [:north :south :west :east]
   :south [:south :west :east :north]
   :west [:west :east :north :south]
   :east [:east :north :south :west]})

(def ^:private cardinal->deltas
  {:north [[-1 -1] [-1 0] [-1 1]]
   :east [[-1 1] [0 1] [1 1]]
   :south [[1 -1] [1 0] [1 1]]
   :west [[-1 -1] [0 -1] [1 -1]]})

(def ^:private cardinal->direction
  {:north [-1 0], :east [0 1], :south [1 0], :west [0 -1]})

(defn- parse-crater [input]
  (into #{} (for [[k v] (u/parse-chart input) :when (= v \#)] k)))

(defn- propose
  "Returns a mapping of position to list of elfs that want to move there."
  [crater first-cardinal]
  (group-by
   (fn [elf]
     (or (when (empty? (u/points crater u/around* elf)) elf)
         (first (for [c (first->cardinals first-cardinal)
                      :when (empty? (u/points crater (cardinal->deltas c) elf))]
                  (u/+p elf (cardinal->direction c))))
         elf))
   crater))

(defn- step [crater first-cardinal]
  (into #{}
        (comp (map (fn [[new olds]] (if (> (count olds) 1) olds [new]))) cat)
        (propose crater first-cardinal)))

(defn- part-1 [simulations]
  (let [crater (nth simulations 11)
        extent (u/chart-extent crater)
        area (* (:width extent) (:height extent))]
    (- area (count crater))))

(defn- part-2 [simulations]
  (->> simulations
       (sequence (comp u/sliding-pair (take-while #(apply distinct? %))))
       count
       inc))

(defn solution [input]
  (let [simulations (reductions step
                                (parse-crater input)
                                (cycle (first->cardinals :north)))]
    [(part-1 simulations) (part-2 simulations)]))

(u/add-solution 23 solution)