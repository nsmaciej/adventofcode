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

(defn- handle-item
  "Peforms a single monkey's operation on the item `x`. The resulting value is
  passed through `(f x)`. Returns a map where :item is the new item value and
  :monkey-id is the monkey the item is to be thrown to."
  [monkey f x]
  (let [new-x (f (case (:op monkey)
                   :square (* x x)
                   :add    (+ x (:arg monkey))
                   :times  (* x (:arg monkey))))
        id (if (zero? (mod new-x (:div-by monkey)))
             (:if-true monkey)
             (:if-false monkey))]
    {:item new-x, :monkey-id id}))

(defn- simulate-turn
  "Perform a single monkey's turn by updating its object in the `troop` transient.
  `f` is passed to `handle-item` as the item value update function."
  [f troop-transient monkey-id]
  (let [monkey (nth troop-transient monkey-id)
        items (:items monkey)
        troop (u/update! troop-transient monkey-id assoc
                         :items []
                         :inspected (+ (:inspected monkey) (count items)))]
    (reduce #(let [{:keys [monkey-id item]} (handle-item monkey f %2)]
               (u/update! %1 monkey-id update :items conj item))
            troop
            items)))

(defn- simulate-round [f troop-transient]
  (reduce #(simulate-turn f %1 %2)
          troop-transient
          (range (count troop-transient))))

(defn- solve [f troop turns]
  (->> troop
       transient
       (iterate #(simulate-round f %1))
       (#(nth % turns))
       persistent!
       (map :inspected)
       (sort >)
       (take 2)
       (apply *)))

(defn- solution [input]
  (let [troop (parse input)
        div-by-lcm (apply u/lcm (map :div-by troop))]
    [(solve #(quot % 3) troop 20)
     (solve #(mod % div-by-lcm) troop 10000)]))

(u/register 11 solution)