(ns ryan.day06
  (:require [clojure.string :as string]))

(def sample-1 "mjqjpqmgbljsphdztnvjfqwrcgsmlb")
(def sample-2 "bvwbjplbgvbhsrlpgdmjqwftvncz")
(def sample-3 "nppdvjthqldpwncqszvftbrmjlhg")
(def sample-4 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")
(def sample-5 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")

(def input (slurp "resources/day06.txt"))

(let [block-size 4]
  (->> input
       (partition block-size 1)
       (map-indexed (fn [i item] {:index i :chars item}))
       (filter (fn [{:keys [chars]}] (= block-size (count (set chars)))))
       first
       :index
       (+ block-size)))