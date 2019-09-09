(ns days.utils
  (:require [clojure.string :as str]))

(defn get-inp [day prod?]
  (->>
    (if prod?
      (slurp (str "resources/data" day ".txt"))
      (slurp (str "resources/data" day ".test.txt")))
    (str/split-lines)))

(defn get-int [s]
  (Integer/parseInt s))