(ns aoc.day21
  (:require [clojure.string :as str]
            [aoc.utils :as u]
            [clojure.core.match :refer [match]]))

(def ^:private parse-op {"+" :plus "-" :minus "*" :times "/" :divide})

(defn- parse-riddle [input]
  (for [line (str/split-lines input)
        :let [[name x op rhs :as words] (str/split line #":? +")]]
    [(keyword name)
     (if (= (count words) 2)
       {:n (parse-long x)}
       {:lhs (keyword x), :op (parse-op op), :rhs (keyword rhs)})]))

(defn- solve [riddle name]
  (let [m (riddle name)]
    (if (:n m)
      (assoc m :val (:n m))
      (let [{x :val :as lhs} (solve riddle (:lhs m))
            {y :val :as rhs} (solve riddle (:rhs m))]
        (assoc m :lhs lhs :rhs rhs :val (if (some #{:humn} [x y])
                                          :humn
                                          (case (:op m)
                                            :plus (+ x y)
                                            :minus (- x y)
                                            :times (* x y)
                                            :divide (/ x y))))))))

(defn- unify [m x]
  (if (= (:n m) :humn)
    x ; We solved it.
    (let [{:keys [lhs op rhs]} m]
      (match [(:val lhs) op (:val rhs)]
        [:humn :plus   y] (recur lhs (- x y))
        [:humn :minus  y] (recur lhs (+ x y))
        [:humn :times  y] (recur lhs (/ x y))
        [:humn :divide y] (recur lhs (* x y))
        [y :plus   :humn] (recur rhs (- x y))
        [y :minus  :humn] (recur rhs (- y x))
        [y :times  :humn] (recur rhs (/ x y))
        [y :divide :humn] (recur rhs (/ y x))))))

(defn- solution [input]
  (let [riddle (into {} (parse-riddle input))]
    [(:val (solve riddle :root))
     (let [s (solve (assoc-in riddle [:humn :n] :humn) :root)]
       (unify (:lhs s) (-> s :rhs :val)))]))

(u/add-solution 21 solution)