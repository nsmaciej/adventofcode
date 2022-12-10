(ns aoc.day02)

(def abc->tool {"A" :rock, "B" :paper, "C" :scissors})
(def xyz->tool {"X" :rock, "Y" :paper, "Z" :scissors})
(def xyz->outcome {"X" :lose, "Y" :draw, "Z" :win})

(def tool->score {:rock 1,
                  :paper 2,
                  :scissors 3})
(def tool->weakness {:rock :paper,
                     :paper :scissors,
                     :scissors :rock})

(defn- score-turn [opp you]
  (+ (tool->score you)
     (condp = you
       (tool->weakness opp) 6
       opp 3
       0)))

(defn- select-tool [opp outcome]
  (case outcome
    :draw opp
    :win (tool->weakness opp)
    :lose (tool->weakness (tool->weakness opp))))

(defn- parse [input]
  (partition 2 (re-seq #"\w" input)))

(defn- score-part-1 [[opp you]]
  (score-turn (abc->tool opp)
              (xyz->tool you)))

(defn- score-part-2 [[opp you]]
  (score-turn (abc->tool opp)
              (select-tool (abc->tool opp)
                           (xyz->outcome you))))

(defn solution [input]
  (let [data (parse input)]
    [(reduce + (map score-part-1 data))
     (reduce + (map score-part-2 data))]))
