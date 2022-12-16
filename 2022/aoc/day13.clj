(ns aoc.day13
  (:require [aoc.utils :as u]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(defn- compare-packets [lhs rhs]
  (if (and (int? lhs) (int? rhs))
    (compare lhs rhs)
    (let [lhs* (if (int? lhs) [lhs] lhs)
          rhs* (if (int? rhs) [rhs] rhs)
          cmps (map compare-packets lhs* rhs*)]
      (or (first (filter (complement zero?) cmps))
          (compare (count lhs*) (count rhs*))))))

(defn part-1 [packets]
  (->> packets
       (partition 2)
       (map #(apply compare-packets %))
       (keep-indexed #(when (neg? %2) (inc %1)))
       (apply +)))

(defn part-2 [packets]
  (let [dividers #{[[2]] [[6]]}]
    (->> packets
         (concat dividers)
         (sort compare-packets)
         (keep-indexed #(when (dividers %2) (inc %1)))
         (apply *))))

(defn- solution [input]
  (let [packets (->> (str/split-lines input)
                     (filter seq)
                     (map edn/read-string))]
    [(part-1 packets) (part-2 packets)]))

(u/register 13 solution)