(ns aoc.day07
  (:require [clojure.string :as str]
            [clojure.core.match :refer [match]]))

(defn- parse-file [line]
  (let [[kind name] (str/split line #" ")]
    (if (= kind "dir")
      {:kind :dir, :name name}
      {:kind :file, :size (parse-long kind)})))

(defn- fs-parse [input]
  (loop [[invocation & rest] (rest (str/split input #"\$ "))
         fs {}   ; List of dirs to file listing mapping
         cwd []] ; List of dirs rooted at []
    (if invocation
      (let [[cmd & output] (str/split-lines invocation)]
        (match (str/split cmd #" ")
          ["cd" "/"]  (recur rest fs [])
          ["cd" ".."] (recur rest fs (pop cwd))
          ["cd" dir]  (recur rest fs (conj cwd dir))
          ["ls"]      (recur rest (assoc fs cwd (mapv parse-file output)) cwd)))
      fs)))

(defn- fs-sizes [fs]
  (let [topo-sorted (reverse (sort (keys fs)))
        dir-size (fn [sizes cwd]
                   (->> (fs cwd)
                        (map #(case (:kind %)
                                :dir (sizes (conj cwd (:name %)))
                                :file (:size %)))
                        (apply +)
                        (assoc sizes cwd)))]
    (reduce dir-size {} topo-sorted)))

(defn- part-1 [sizes]
  (->> sizes
       vals
       (filter #(<= % 100000))
       (apply +)))

(defn- part-2 [sizes]
  (let [taken-current (sizes [])
        need-to-free (- taken-current 40000000)]
    (->> sizes
         vals
         (filter #(>= % need-to-free))
         (apply min))))

(defn solution [input]
  (let [sizes (fs-sizes (fs-parse input))]
    [(part-1 sizes) (part-2 sizes)]))