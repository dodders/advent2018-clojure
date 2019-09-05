(ns days.day01
  (:require [clojure.string :as str]))

(defn parse-int [s]
  (Integer/parseInt s))

(defn get-ops []
  (map parse-int (str/split (slurp "resources/data01.txt") #"\n")))

(defn part1 []
  (apply + (get-ops)))


(defn do-part2 [hist sum ops]
  (loop [hist hist
         sum sum
         ops ops]
    (let [new-sum (+ sum (first ops))]
      (do
        ;(println hist)
        (printf "%5d %5d %5d%n" sum (first ops) new-sum)
        (if (contains? hist new-sum)
          new-sum
          (recur (conj hist new-sum) new-sum (rest ops)))))))

(defn part2 []
  (do-part2 #{} 0 (take 1000000 (cycle (get-ops)))))