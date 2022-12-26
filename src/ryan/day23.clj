(ns ryan.day23
  (:require [clojure.string :as str]))

(defn read-elves [file]
  (->> file
       slurp
       str/split-lines
       (map-indexed (fn [y line]
                      (map-indexed (fn [x c]
                                     [[x y] c])
                                   line)))
       (mapcat identity)
       (reduce (fn [acc [p c]]
                 (if (= c \#)
                   (conj acc p)
                   acc))
               #{})))

(defn look-west [[px py]]
  [[(dec px) (inc py)]
   [(dec px) py]
   [(dec px) (dec py)]])

(defn look-east [[px py]]
  [[(inc px) (inc py)]
   [(inc px) py]
   [(inc px) (dec py)]])

(defn look-south [[px py]]
  [[(dec px) (inc py)]
   [px (inc py)]
   [(inc px) (inc py)]])

(defn look-north [[px py]]
  [[(dec px) (dec py)]
   [px (dec py)]
   [(inc px) (dec py)]])

(comment 
  (look-west [1 3])
  (look-east [1 3])
  (look-south [1 3])
  (look-north [1 3])
  
  (#(second (look-north %)) [1 3])
  ;
  )

(def move-order [{:look look-north :next #(second (look-north %))}
                 {:look look-south :next #(second (look-south %))}
                 {:look look-west :next #(second (look-west %))}
                 {:look look-east :next #(second (look-east %))}])

(defn scatter-elves [{grid :grid
                      [{:keys [look next]} & others] :moves} _]
  (let [next-moves (map (fn [p]
                          (let [[l1 l2 l3] (look p)]
                            (if (or (grid l1) (grid l2) (grid l3))
                              [p p]
                              [p (next p)])))
                        grid)
        next-moves-set (set (map second next-moves))]
    (reduce (fn [acc [from to]]
              (cond
                (= from to) (conj acc from)
                (next-moves-set to) (conj acc from)
                :else (conj acc to)))
            #{}
            next-moves)))

(comment 
  (scatter-elves {:grid (read-elves "resources/day23-sample-simple.txt")
                  :moves move-order}
                 nil)
  ;
  )

(read-elves "resources/day23-sample.txt")
