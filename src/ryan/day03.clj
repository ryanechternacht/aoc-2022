(ns ryan.day03
  (:require [clojure.string :as str]
            [clojure.set :as set]))
  
(defn read-data-1 [file]
  (->> file
       slurp
       (#(str/split % #"\n"))
       (map (fn [line]
              (let [halfway (/ (count line) 2)]
                {:half-1 (subs line 0 halfway)
                 :half-2 (subs line halfway)
                 :line line})))))

(defn get-matching-char [{:keys [half-1 half-2]}]
  (let [s1 (-> half-1 frequencies keys set)
        s2 (-> half-2 frequencies keys set)]
    (first (set/intersection s1 s2))))

(defn get-priority [c]
  (if (Character/isLowerCase c)
    (+ 1 (- (int c) (int \a)))
    (+ 27 (- (int c) (int \A)))))

(->> "resources/day03.txt"
     read-data-1
     (map get-matching-char)
     (map get-priority)
     (reduce +))

(defn read-data-2 [file]
  (->> file
       read-data-1
       (partition 3)))

(defn find-badge-char [group]
  (->> group
       (map :line)
       (map #(-> % frequencies keys set))
       (apply set/intersection)
       first))

(->> "resources/day03.txt"
     read-data-2
     (map find-badge-char)
     (map get-priority)
     (reduce +))