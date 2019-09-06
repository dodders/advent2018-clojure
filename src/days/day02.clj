(ns days.day02
  (:require [clojure.string :as str]))

(defn get-inp []
  (->>
    ;(slurp "resources/data02.test.txt")
    (slurp "resources/data02.txt")
    (str/split-lines)))

(defn part1-calc [items]
  (let [twos (count (filter #{2} items))
        threes (count (filter #{3} items))]
    (* twos threes)))

(defn part1 []
  (->> (for [inp (get-inp)]
         (->>
           (frequencies inp)
           (vals)
           (filter #{2 3})
           (sort)
           (dedupe)))
       (flatten)
       (part1-calc)))

(defn match-chars [f w1 w2]
  ;(prn w1 w2)
  (->>
    (map vector w1 w2)
    (filter (fn [[x y]] (f x y)))))

(defn part2 []
  (let [inp (get-inp)]
    (->>
      (for [lhs inp
            rhs inp
            :when (= 1 (count (match-chars not= lhs rhs)))]
        (match-chars = lhs rhs))
      (first)
      (map first)
      (apply str))))

(defn run []
  {:part1 (part1)
   :part2 (part2)})
