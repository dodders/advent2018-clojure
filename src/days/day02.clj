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



(defn run []
  {:part1 (part1)
   :part2 (part2)})
