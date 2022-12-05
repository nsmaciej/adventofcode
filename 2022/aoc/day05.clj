(ns aoc.day05
  (:require [clojure.string :as str]))

(defn- parse-pic [pic]
  (->> pic
       str/split-lines
       drop-last             ; Remove the number line
       (map rest)            ; Make letters the first character
       (map #(take-nth 4 %)) ; Extract only the letters or spaces
       (apply map vector)    ; Transpose
       (mapv #(remove #{\space} %))))

(defn- parse [input]
  (let [[pic actions] (str/split input #"\n\n")]
    [(parse-pic pic)
     (->> actions
          (re-seq #"\d+")
          (map parse-long)
          (partition 3))]))

(defn- apply-step [f pic [n from to]]
  (let [from-1 (dec from)
        to-1 (dec to)
        [taken leftover] (split-at n (nth pic from-1))]
    (-> pic
        (assoc from-1 leftover)
        (update to-1 #(concat (f taken) %)))))

(defn- run-crane [f [pic steps]]
  (->> steps
       (reduce #(apply-step f %1 %2) pic)
       (map first)
       (apply str)))

(defn solution [input]
  (let [data (parse input)]
    [(run-crane reverse data)
     (run-crane identity data)]))

(comment
  (def sample-pic
    (parse-pic (str "[D]        \n"
                    "[N] [C]    \n"
                    "[Z] [M] [P]\n"
                    " 1   2   3"))))