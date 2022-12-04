(ns ryan.day01
  (:require [clojure.string :as str]))

(defn read-data [file]
  (->> file
       slurp
       (#(str/split % #"\n"))
       (partition-by #(= % ""))
       (remove #(= % '("")))
       (reduce (fn [acc l]
                 (conj acc
                       (->> l
                            (map parse-long)
                            (reduce +))))
               [])))

(->> "resources/day01.txt"
     read-data
     (reduce max))

(->> "resources/day01.txt"
     read-data
     sort
     reverse
     (take 3)
     (reduce +))