(ns days.day02
  (:require [clojure.string :as str]))

(defn get-inp []
  (str/split (slurp "resources/data02.test.txt") #"\n"))
  ;(str/split (slurp "resources/data02.txt") #"\n"))

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

(defn diff-by-one? [s1 s2 diff]
  (cond
    (> diff 1) false
    (and (= 0 (count s1)) (= diff 0)) false
    (and (= 0 (count s1)) (= diff 1)) true
    :else (diff-by-one? (rest s1) (rest s2)
                        (if (= (first s1) (first s2)) diff (inc diff)))))

(defn get-common-letters [w1 w2 ret]
  (cond
    (= 0 (count w1)) ret
    (= (first w1) (first w2))
    (get-common-letters (rest w1) (rest w2) (conj ret (first w1)))
    :else  (get-common-letters (rest w1) (rest w2) ret)))

; return a function that finds a match for word in words
(defn get-match-fn [words]
  (fn [word]
    (prn word words)))


(defn part2 []
  (let [inp (get-inp)
        match (get-match-fn inp)]
    (map match inp)))

