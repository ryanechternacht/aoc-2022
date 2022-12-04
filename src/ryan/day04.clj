(ns ryan.day04
    (:require [clojure.string :as str]))

(defn read-data [file]
  (->> file
       slurp
       (#(str/split % #"\n"))
       (map (fn [line] 
              (->> line
                   (#(str/split % #","))
                   (map #(str/split % #"-"))
                   (map (fn [[x y]]
                          {:lower (Integer/parseInt x)
                           :upper (Integer/parseInt y)})))))))

(defn check-for-containment [[{l1 :lower u1 :upper} {l2 :lower u2 :upper}]]
  (or ; 1 contains 2
   (and (<= l1 l2) (>= u1 u2))
   ; 2 contains 1
   (and (<= l2 l1) (>= u2 u1))))
  
(->> "resources/day04.txt"
     read-data
     (map check-for-containment)
     (filter identity)
     count)

(defn check-for-overlap [[{l1 :lower u1 :upper} {l2 :lower u2 :upper}]]
  (and (<= l1 u2) (<= l2 u1)))

(->> "resources/day04.txt"
     read-data
     (map check-for-overlap)
     (filter identity)
     count)
