(ns days.day03
  (:require [days.utils :as utils])
  (:require [clojure.string :as strings]))

(defn get-points [coord]
    (vector
      (vec (map utils/get-int (strings/split (strings/replace (coord 2) ":" "") #",")))
      (vec (map utils/get-int (strings/split (coord 3) #"x")))
      (coord 0)))

; expand (x,y) and (2x2) into
; (x,y x+1,y x,y+1 x+1,y+1)
(defn expand [coord]
  (let [point (coord 0)
        area (coord 1)
        claim (coord 2)]
      (for [x (range (point 0) (+ (point 0) (area 0)))
            y (range (point 1) (+ (point 1) (area 1)))]
          (vector x y claim))))

(defn part1 [prod]
  (->>
    (utils/get-inp "03" prod)
    (map #(strings/split % #" "))
    (map get-points)
    (map expand)
    (apply concat)
    (map #(vector (% 0) (% 1)))
    (frequencies)
    (filter #(> (val %) 1))
    (count)))

(defn part2 [prod]
  (let [points
        (->> (utils/get-inp "03" prod)
          (map #(strings/split % #" "))
          (map get-points))
        overlaps
        (->> (map expand points)
             (apply concat)
             (group-by #(vector (% 0) (% 1)))
             (vals)
             (filter #(> (count %) 1))
             (apply concat)
             (map #(% 2))
             (distinct)
             (apply hash-set))
        actual-points (map #(% 2) points)]
    (filter #(not (overlaps %)) actual-points)))

(defn run []
  (let [prod true]
    {:part1 (part1 prod)
     :part2 (part2 prod)}))
