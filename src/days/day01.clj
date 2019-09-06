(ns days.day01
  (:require [clojure.string :as str]))

(defn parse-int [s]
  (Integer/parseInt s))

(defn get-ops []
  (map parse-int (str/split-lines (slurp "resources/data01.txt"))))

(defn part1 []
  (apply + (get-ops)))


(defn do-part2 [hist sum ops]
  (loop [hist hist
         sum sum
         ops ops]
    (let [new-sum (+ sum (first ops))]
      (do
        (if (contains? hist new-sum)
          new-sum
          (recur (conj hist new-sum) new-sum (rest ops)))))))

(defn part2 []
  (do-part2 #{} 0 (take 1000000 (cycle (get-ops)))))

(defn run []
  {:part1 (part1)
   :part2 (part2)})