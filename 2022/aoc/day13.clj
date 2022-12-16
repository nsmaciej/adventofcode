(ns aoc.day13
  (:require [aoc.utils :as u]
            [clojure.string :as str]))

(defn- compare-packets [lhs rhs]
  (if (and (int? lhs) (int? rhs))
    (compare lhs rhs)
    (let [lhs* (if (seqable? lhs) lhs [lhs])
          rhs* (if (seqable? rhs) rhs [rhs])
          cmps (map compare-packets lhs* rhs*)]
      (or (first (filter (complement zero?) cmps))
          (compare (count lhs*) (count rhs*))))))

(defn- compare-pair [pair]
  (->> (str/split-lines pair)
       (map read-string)
       (apply compare-packets)))

(->> (str/split (u/input 13) #"\n\n")
     (map compare-pair)
     (keep-indexed #(when (<= %2 0) (inc %1)))
     (apply +))