(ns aoc.day21
  (:require [clojure.string :as str]
            [aoc.utils :as u]
            [clojure.core.match :refer [match]]))

(def ^:private parse-op {"+" :plus "-" :minus "*" :times "/" :divide})

(defn- parse-riddle [input]
  (into
   {}
   (for [line (str/split-lines input)]
     (let [[name x op rhs :as words] (str/split line #":? +")]
       [(keyword name)
        (if (= (count words) 2)
          {:n (parse-long x)}
          {:lhs (keyword x), :op (parse-op op), :rhs (keyword rhs)})]))))

(defn- solve [riddle name]
  (let [m (riddle name)]
    (if (contains? m :n)
      (assoc m :name name, :value (:n m))
      (let [{x :value :as lhs} (solve riddle (:lhs m))
            {y :value :as rhs} (solve riddle (:rhs m))]
        (assoc m
               :name name :lhs lhs :rhs rhs
               :value (if (and x y)
                        (case (:op m)
                          :plus (+ x y)
                          :minus (- x y)
                          :times (* x y)
                          :divide (/ x y))
                        nil))))))

(defn- unify [m x]
  (if (= (:name m) :humn)
    x ; We solved it.
    (let [{:keys [lhs op rhs]} m]
      (match [(:value lhs) op (:value rhs)]
        [nil :plus   y] (recur lhs (- x y))
        [nil :minus  y] (recur lhs (+ x y))
        [nil :times  y] (recur lhs (/ x y))
        [nil :divide y] (recur lhs (* x y))
        [y :plus   nil] (recur rhs (- x y))
        [y :minus  nil] (recur rhs (- y x))
        [y :times  nil] (recur rhs (/ x y))
        [y :divide nil] (recur rhs (/ y x))))))

(defn- solution [input]
  (let [riddle (parse-riddle input)]
    [(:value (solve riddle :root))
     (let [s (solve (assoc-in riddle [:humn :n] nil) :root)]
       (unify (:lhs s) (-> s :rhs :value)))]))

(u/add-solution 21 solution)
(u/solve 21)