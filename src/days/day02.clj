(ns days.day02
  (:require [clojure.string :as str]))

(defn get-inp []
  ;(str/split (slurp "resources/data02.test.txt") #"\n"))
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

; true if the two words are different by one character.
(defn diff-by-one? [w1 w2 diff]
  (cond
    (> diff 1) false
    (and (empty? w1) (= diff 0)) false
    (and (empty? w2) (= diff 1)) true
    :else (diff-by-one? (rest w1) (rest w2)
                        (if (= (first w1) (first w2)) diff (inc diff)))))

; return only the common letters between words
(defn get-common-letters [w1 w2 ret]
  (cond
    (= 0 (count w1)) ret
    (= (first w1) (first w2))
    (get-common-letters (rest w1) (rest w2) (conj ret (first w1)))
    :else  (get-common-letters (rest w1) (rest w2) ret)))

; find a match for word in words
(defn find-match [word words]
  (cond
    (empty? words) false
    (diff-by-one? word (first words) 0) (list word (first words))
    :else (find-match word (rest words))))

; create closure of words so we can use find-match as a filter
(defn get-filter [words]
  (fn [word]
    (find-match word words)))

; find all matches in word list (should reduce to a single pair of words)
(defn get-matches []
  (let [inp (get-inp)
        match-filter (get-filter inp)]
    ;reduce list of words to a list of matching pairs
    (filter match-filter inp)))

(defn part2 []
  (let [matches (get-matches)]
    (reduce str (get-common-letters (first matches) (second matches) []))))



