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

(defn part1 [prod]
  (->>
    (get-inp prod)
    (do-react)
    (count)))


(defn run []
  (let [prod true]
    {:part1 (part1 prod)}))

(defn red [x y]
  (do
    (prn x y)
    (if (> y 4)
      (pop x)
      (conj x y))))
