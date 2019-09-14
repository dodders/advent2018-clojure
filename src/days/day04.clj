(ns days.day04
  (:require [days.utils :as utils])
  (:require [clojure.string :as strings]
            [clojure.string :as str]))

(defn parse-inp [inp guard]
  (let [line (first inp)
        line-type (subs line 19 24)
        line-guard (if (= line-type "Guard") (subs line 25 28) guard)
        new-guard (if (= line-guard guard) guard line-guard)
        hrs (Integer/parseInt(subs line 12 14))
        mins (Integer/parseInt(subs line 15 17))
        rec {:guard new-guard :action line-type :hrs hrs :mins mins}]
    (if (= 1 (count inp))
      (list rec)
      (cons rec (parse-inp (rest inp) new-guard)))))

; return list of guards with the guard sleep/awake minutes applied
; ignores anything other than falls/wakes.
(defn add-mins [guards row]
  (case (row :action)
    "falls" (assoc guards (row :guard) (+ (guards (row :guard) 0) (unchecked-negate-int (row :mins))))
    "wakes" (assoc guards (row :guard) (+ (guards (row :guard) 0) (row :mins)))
    guards))

; find the guard with the most minutes asleep.
(defn guards-total-sleep [shed]
  (loop [inp shed
         guards {}]
      (cond
        (empty? inp) guards
        :else (recur (rest inp) (add-mins guards (first inp))))))

(defn sleepiest-guard [sleeps]
  (key (apply max-key val sleeps)))

(defn expand [schedule]
  (case (schedule :action)
    "falls" ()
    "wakes" ()))

(defn minute-most-asleep [guard schedule]
  (let [filtered (filter #(= guard (% :guard)) schedule)]
    filtered))

(defn part1 [prod]
  (let [inp (sort (utils/get-inp "04" prod))
        schedule (parse-inp inp nil) 
        tired-guard (sleepiest-guard (guards-total-sleep schedule))]
    (minute-most-asleep tired-guard schedule)))

(defn run []
  (let [prod false]
    {:part1 (part1 prod)}))
;:part2 (part2 prod)}))

;(def d (part1 false))
(def inp (parse-inp (sort (utils/get-inp "04" false)) nil))


