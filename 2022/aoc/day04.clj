(ns aoc.day04)

(defn- parse [input]
  (partition 4 (map parse-long (re-seq #"\d+" input))))

(defn- full-overlap? [[a x b y]]
  (or (and (<= a b) (>= x y))
      (and (<= b a) (>= y x))))

(defn- partial-overlap? [[a x b y]]
  (not (or (< x b) (< y a))))

(defn solution [input]
  (let [data (parse input)]
    [(count (filter full-overlap? data))
     (count (filter partial-overlap? data))]))