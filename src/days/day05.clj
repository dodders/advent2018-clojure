(ns days.day05
  (:require [days.utils :as utils])
  (:require [clojure.string :as strings]
            [clojure.string :as str]))

(defn get-inp [prod]
  (utils/get-inp "05" prod))

(defn upper [s]
  (if s (str/upper-case s) nil))

; equality function for units
(defn units= [x y]
  (let [ux (upper x)
        uy (upper y)]
    (and (= ux uy) (or (not= x y) (not= ux uy)))))

(defn react-polymer [poly]
  (reduce
    (fn [res letter]
      (if (units= (peek res) letter)
        (pop res)
        (conj res letter)))
    [] poly))

; keep reacting a polymer until it's nil or no changes.
(defn do-react [poly]
    (let [reacted (apply str (react-polymer poly))]
      (if (or (= reacted poly) (empty? reacted))
        reacted
        (recur reacted))))

(defn not-match-ignore-case [a b]
  (not (= (upper a) (upper b))))

(defn do-unit-types [poly]
  (for [x "abcdefghijklmnopqrstuvwxyz"]
     (let [new-poly (filter #(not-match-ignore-case x %) poly)
           length (count (do-react new-poly))]
      {x length})))

(defn part2 [inp]
  (->>
    (do-unit-types inp)
    (reduce conj)
    (apply min-key val)))

(defn part1 [inp]
    (count (do-react inp)))

(defn run []
  (let [prod true
        inp (get-inp prod)]
    {:part1 (part1 inp)
     :part2 (part2 (inp 0))}))

