(ns aoc.day11
  (:require [instaparse.core :as insta :refer [defparser]]
            [aoc.utils :as u]))

(defparser parse-notes
  "notes = monkey*
   monkey = <'Monkey'> number <cln> items op div-by
   items = <'Starting items'> <cln> number (<','> number)*
   op = <'Operation'> <cln> <'new'> <'='> <'old'> ('*' ('old' | number) | '+' number)
   div-by = <'Test'> <cln> <'divisible'> <'by'> number if if
   if = <'If'> ('true' | 'false') <cln> <'throw'> <'to'> <'monkey'> number
   cln = ':'
   <number> = #'[0-9]+'"
  :auto-whitespace :standard)

(defn parse [input]
  (insta/transform
   {:notes #(vec (sort-by :id %&)) ; We don't need to assume the order of the monkeys.
    :monkey #(merge {:id (parse-long %1) :items %2 :inspected 0} %3 %4)
    :op #(cond (= %1 "+")   {:op :add, :arg (parse-long %2)}
               (= %2 "old") {:op :square}
               :else        {:op :times, :arg (parse-long %2)})
    :items #(mapv parse-long %&)
    :div-by #(merge {:div-by (parse-long %1)} %2 %3)
    :if #(hash-map (if (= %1 "true") :if-true :if-false) (parse-long %2))}
   (parse-notes input)))

(defn- monkey-turn [monkey x]
  (let [x-op (case (:op monkey)
               :square (* x x)
               :add    (+ x (:arg monkey))
               :times  (* x (:arg monkey)))
        x-relief (quot x-op 3)
        id (if (zero? (mod x-relief (:div-by monkey)))
             (:if-true monkey)
             (:if-false monkey))]
    {:item x-relief, :monkey id}))

(defn- troop-turn [troop monkey x]
  (let [turn (monkey-turn monkey x)]
    ;; (println "Passing" turn
    ;;          (update-in troop [(:monkey turn) :items] #(conj % (:item turn))))
    (update-in troop [(:monkey turn) :items] conj (:item turn))))

; Troop is a group of monkeys.
(defn- play-round [result x]
  (let [monkey (nth result x)]
    (reduce #(troop-turn %1 monkey %2)
            (update result x assoc
                    :items []
                    :inspected (+ (:inspected monkey) (count (:items monkey))))
            (:items monkey))))

(->> 20
     (nth (iterate #(reduce play-round % (range (count %))) (parse (u/input 11))))
     (map :inspected)
     (sort >)
     (take 2)
     (apply *))

(comment
  (def sample-input "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1"))