(ns ryan.day13
  (:require [clojure.string :as str]))

(read-string "[1,[2,[3,[4,[5,6,7]]]],8,9]")

(zipmap (read-string "[1,[2,[3,[4,[5,6,7]]]],8,9]")
        (read-string "[1,[2,[3,[4,[5,6,0]]]],8,9]"))

(defn read-pairs [file]
  (->> file
       slurp
       (str/split-lines)
       (remove str/blank?)
       (map read-string)
       (partition 2)))

; -1 = correct (l < r), 1 = incorrect (r > l), 0 = keep going (l = r)
(defn is-correct-order? [left right]
  (loop [[l & l-others] left
         [r & r-others] right]
    (cond
      (and (number? l) (number? r))
      (if (= l r)
        (recur l-others r-others)
        (compare l r))

      (and (vector? l) (vector? r))
      (condp = (is-correct-order? l r)
        0 (recur l-others r-others)
        1 1
        -1 -1)

      (and (vector? l) (number? r))
      (condp = (is-correct-order? l [r])
        0 (recur l-others r-others)
        1 1
        -1 -1)

      (and (number? l) (vector? r))
      (condp = (is-correct-order? [l] r)
        0 (recur l-others r-others)
        1 1
        -1 -1)

      (and (nil? l) (nil? r)) 0

      (nil? l) -1

      (nil? r) 1
      ;
      )))

(comment
  (is-correct-order? [1 1 3 1 1] [1 1 5 1 1])
  (is-correct-order? [1 2 3 1 1] [1 1 5 1 1])

  (is-correct-order? [[1] [2 3 4]] [[1] 4])
  (is-correct-order? [9] [[8 7 6]])

  (is-correct-order? [] [3])
  (is-correct-order? [3 4] [3])
  (is-correct-order? [[[]]] [[]])
  )

;; part 1
(->> "resources/day13.txt"
     read-pairs
     (map (fn [[l r]] (is-correct-order? l r)))
     (map-indexed (fn [i c] [(inc i) c]))
     (filter #(neg? (second %)))
     (map first)
     (reduce +))

;; part 2
(->> "resources/day13.txt"
     slurp
     (str/split-lines)
     (remove str/blank?)
     (map read-string)
     (concat [[[2]] [[6]]])
     (sort is-correct-order?)
     (map-indexed (fn [i c] [(inc i) c]))
     (filter #(#{[[2]] [[6]]} (second %)))
     (map first)
     (reduce *))


;; compare lists fn
;;   built with loop/recur (I think you could do reduce/reduced)
;;   big cond
;;     if 2 nums, compare (return 1, 0, -1)
;;     if 2 lists, recurse on the lists
;;     if list and num, make the num a list then recurse on the lists
;;     if 1 missing, then return 1, -1
;;   ^ on 1/-1 return, on 0 keep going
