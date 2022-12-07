(ns aoc.day07
  (:require [clojure.string :as str]))

(defn- parse-file [line]
  (let [[kind name] (str/split line #" ")]
    (if (= kind "dir")
      {:kind :dir, :name name}
      {:kind :file, :name name, :size (parse-long kind)})))

(defn- parse-output [text]
  (let [[cmd & output] (str/split-lines text)
        [program & args] (str/split cmd #" ")]
    (case program
      "cd" {:cmd :cd, :where (first args)}
      "ls" {:cmd :ls, :listing (mapv parse-file output)})))

(defn- parse [input]
  (->> #"\$ "
       (str/split input)
       rest
       (map parse-output)))

(defn- fs-make [data]
  (loop [cwd [], fs {}, [cmd & rest] data]
    (case (:cmd cmd)
      :cd (let [new-cmd (case (:where cmd)
                          "/" []
                          ".." (pop cwd)
                          (conj cwd (:where cmd)))]
            (recur new-cmd fs rest))
      :ls (recur cwd (assoc fs cwd (:listing cmd)) rest)
      fs)))

(defn- fs-reduce
  "Given a mapping paths and values, produces a new mapping by invoking f with
  the new mapping with all topological predecessors and the current path."
  [f fs]
  (->> fs
       keys
       sort
       reverse
       (reduce #(assoc %1 %2 (f %1 %2)) {})))

(defn- fs-sizes [fs]
  (letfn [(sum-children [sizes cwd]
            (->> (fs cwd)
                 (map #(case (:kind %)
                         :dir (sizes (conj cwd (:name %)))
                         :file (:size %)))
                 (reduce +)))]
    (fs-reduce sum-children fs)))

(defn solution [input]
  (let [fs (fs-make (parse input))
        sizes (fs-sizes fs)
        taken-max (- 70000000 30000000)
        taken-current (sizes [])
        need-to-free (- taken-current taken-max)]
    [(->> sizes
          vals
          (filter #(<= % 100000))
          (reduce +))
     (->> sizes
          vals
          (filter #(>= % need-to-free))
          sort
          first)]))
