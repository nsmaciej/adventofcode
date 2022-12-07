(ns aoc.day07
  (:require [clojure.string :as str]))

(defn- parse-file [line]
  (let [[kind name] (str/split line #" ")]
    (if (= kind "dir")
      {:kind :dir, :name name}
      {:kind :file, :size (parse-long kind)})))

(defn- fs-parse [input]
  (loop [[group & groups] (rest (str/split input #"\$ "))
         fs {}   ; List of dirs to file listing mapping
         cwd []] ; List of dirs rooted at []
    (if group
      (let [[cmd & output] (str/split-lines group)
            [program arg] (str/split cmd #" ")]
        (case program
          "cd" (recur groups
                      fs
                      (case arg "/" [], ".." (pop cwd), (conj cwd arg)))
          "ls" (recur groups
                      (assoc fs cwd (mapv parse-file output))
                      cwd)))
      fs)))

(defn- fs-sizes [fs]
  (let [topo-sorted (reverse (sort (keys fs)))
        dir-size (fn [sizes cwd]
                   (->> (fs cwd)
                        (map #(case (:kind %)
                                :dir (sizes (conj cwd (:name %)))
                                :file (:size %)))
                        (reduce +)
                        (assoc sizes cwd)))]
    (reduce dir-size {} topo-sorted)))

(defn solution [input]
  (let [fs (fs-parse input)
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
          (apply min))]))