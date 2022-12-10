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

(defn- parse
  "Parse into a list of container stacks (top first) and required steps."
  [input]
  (let [[pic actions] (str/split input #"\n\n")
        triples (->> actions
                     (re-seq #"\d+")
                     (map parse-long)
                     (partition 3))]
    [(parse-pic pic)
     (for [[n from to] triples]
       {:amount n, :from (dec from), :to (dec to)})]))

(defn- apply-step [f stacks {:keys [to from amount]}]
  (let [[taken leftover] (split-at amount (nth stacks from))
        target (nth stacks to)]
    (assoc stacks
           from leftover
           to (concat (f taken) target))))

(defn- run-crane [f [stacks steps]]
  (->> steps
       (reduce #(apply-step f %1 %2) stacks)
       (map first)
       str/join))

(defn solution [input]
  (let [data (parse input)]
    [(run-crane reverse data)
     (run-crane identity data)]))

(comment
  (parse-pic (str "[D]        \n"
                  "[N] [C]    \n"
                  "[Z] [M] [P]\n"
                  " 1   2   3")))