(ns days.day02
  (:require [clojure.string :as str]))

(defn get-inp []
  ;(str/split (slurp "resources/data02.test.txt") #"\n")
  (str/split (slurp "resources/data02.txt") #"\n"))

(defn same [x] x)

; return 1 if any letters in letters occur exactly ct times
(defn counts [ct letters]
  (let [grouped (group-by same letters) ; group by itself
        total (count (filter #(= ct (count %)) (vals grouped)))] ; count occurrences
    (if (>= total 1) 1 0))) ; only return 1 or 0

; reduce a list of letters to [x y] where
; x is 1 if any letters occur twice, and
; y is 1 if any letters occur thrice
(defn twos-and-threes [letters]
  [(counts 2 letters) (counts 3 letters)])

; reduce [two three] counts to a total of [twos threes]
(defn get-totals [sum item]
  [(+ (sum 0) (item 0)) (+ (sum 1) (item 1))])

(defn part1 []
  (let [letter-counts (map twos-and-threes (get-inp))
        total (reduce get-totals letter-counts)]
    ; total is [x y] where x is sum of two counts and y is sum
    ; of three counts, so answer is x * y...
    (reduce * total)))

(part1)



