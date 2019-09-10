(ns days.day03
  (:require [days.utils :as utils])
  (:require [clojure.string :as strings]))

(defn get-points [coord]
    (list
      (vec (map utils/get-int (strings/split (strings/replace (coord 2) ":" "") #",")))
      (vec (map utils/get-int (strings/split (coord 3) #"x")))))

; expand (x,y) and (2x2) into
; (x,y x+1,y x,y+1 x+1,y+1)
(defn expand [coord]
  (let [point (first coord)
        area (second coord)]
      (for [x (range (point 0) (+ (point 0) (area 0)))
            y (range (point 1) (+ (point 1) (area 1)))]
          (list x y))))

(defn part1 [prod]
  (->>
    (utils/get-inp "03" prod)
    (map #(strings/split % #" "))
    (map get-points)
    (map expand)
    (apply concat)
    (frequencies)
    (filter #(> (val %) 1))
    (count)))

(defn run []
  (let [prod true]
    {:part1 (part1 prod)}))

