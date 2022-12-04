(ns ryan.day02
  (:require [clojure.string :as str]))

(defn read-data [file]
  (->> file
       slurp
       (#(str/split % #"\n"))
       (map #(str/split % #" "))))

(defn score-game-1 [game]
  (condp = game
    ["A" "X"] (+ 1 3)
    ["A" "Y"] (+ 2 6)
    ["A" "Z"] (+ 3 0)
    ["B" "X"] (+ 1 0)
    ["B" "Y"] (+ 2 3)
    ["B" "Z"] (+ 3 6)
    ["C" "X"] (+ 1 6)
    ["C" "Y"] (+ 2 0)
    ["C" "Z"] (+ 3 3)
    nil))

(defn score-game-2 [game]
  (condp = game
    ["A" "X"] (+ 3 0) ; scissors
    ["A" "Y"] (+ 1 3) ; rock
    ["A" "Z"] (+ 2 6) ; paper
    ["B" "X"] (+ 1 0) ; rock
    ["B" "Y"] (+ 2 3) ; paper 
    ["B" "Z"] (+ 3 6) ; scissors
    ["C" "X"] (+ 2 0) ; paper
    ["C" "Y"] (+ 3 3) ; scissors
    ["C" "Z"] (+ 1 6) ; rock
    nil))

(->> "resources/day02.txt"
     read-data
     (map score-game-2)
     (reduce +))