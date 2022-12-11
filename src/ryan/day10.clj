(ns ryan.day10
  (:require [clojure.string :as str]))

(defn read-commands [file]
  (->> file
       slurp
       str/split-lines
       (map #(str/split % #" "))
       (map (fn [[cmd n]]
              (condp = cmd
                "addx" {:cmd :addx
                        :num (Integer/parseInt n)}
                "noop" {:cmd :noop})))))

(defn add-signal-strengths [{:keys [strengths current] :as acc} {:keys [cmd num]}]
  (let [prior (last strengths)]
    (condp = cmd
      :noop (-> acc
                (update :strengths conj current))
      :addx (-> acc
                (update :strengths conj current current)
                (update :current + num)))))

(comment
  (add-signal-strengths {:strengths [1 2 3]
                         :current 5}
                        {:cmd :noop})
  (add-signal-strengths {:strengths [1 2 3]
                         :current 5}
                        {:cmd :addx
                         :num 7})
  ;
  )

(def pull-desired-readings
  (->> (map #(+ 20 (* 40 %)) (range 6))
       (map (fn [x]
              (fn [coll]
                (* x (nth coll (dec x))))))
       (apply juxt)))

; part 1
(->> "resources/day10.txt"
     read-commands
     (reduce add-signal-strengths
             {:strengths []
              :current 1})
     :strengths
     pull-desired-readings
     (reduce +))

; part 2
(->> "resources/day10.txt"
     read-commands
     (reduce add-signal-strengths
             {:strengths []
              :current 1})
     :strengths
     (partition 40)
     (map #(map-indexed (fn [i s]
                          (if (<= (dec i) s (inc i))
                            "#"
                            ".")) %))
     (map str/join))
