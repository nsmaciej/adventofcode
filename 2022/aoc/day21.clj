(ns aoc.day21
  (:require [clojure.string :as str]
            [aoc.utils :as u]))

(def ^:private parse-op {"+" :plus "-" :minus "*" :times "/" :divide})

(defn- parse-riddle [input]
  (into {}
        (for [line (str/split-lines input)]
          (let [[name x op rhs :as words] (str/split line #":? +")]
            (if (= (count words) 2)
              [(keyword name) {:n (parse-long x)}]
              [(keyword name) {:lhs (keyword x) :op (parse-op op) :rhs (keyword rhs)}])))))

(defn- solve [riddle name]
  (let [m (riddle name)]
    (or (:n m)
        (let [lhs (solve riddle (:lhs m))
              rhs (solve riddle (:rhs m))]
          (case (:op m)
            :plus (+ lhs rhs)
            :minus (- lhs rhs)
            :times (* lhs rhs)
            :divide (/ lhs rhs))))))

(comment
  (def sample "root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32")

  (solve (parse-riddle (u/input 21)) :root)
  ;
  )