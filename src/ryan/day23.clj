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

(defn find-next-move [grid moves p]
  (let [surroundings (reduce into #{} [(look-west p)
                                       (look-east p)
                                       (look-north p)
                                       (look-south p)])]
    (if (not-any? #(grid %) surroundings)
      p
      (reduce (fn [_ {:keys [look next]}]
                (let [[l1 l2 l3] (look p)]
                  (if (or (grid l1) (grid l2) (grid l3))
                    p
                    (reduced (next p)))))
              p
              moves))))

(defn scatter-elves [{:keys [grid moves]} _]
  (let [next-moves (map (fn [p]
                          [p (find-next-move grid moves p)])
                        grid)
        new-grid (reduce (fn [acc [from to]]
                           (let [next-moves-set (->> next-moves
                                                     (remove (fn [[f _]] (= from f)))
                                                     (map second)
                                                     set)]
                             (cond
                               (= from to) (do #_(println "match") (conj acc from))
                               (next-moves-set to) (do #_(println "crash!") (conj acc from))
                               :else (do #_(println "move")  (conj acc to)))))
                         #{}
                         next-moves)]
    {:grid new-grid
     :moves (conj (vec (drop 1 moves)) (first moves))}))

(comment
  (def simple-grid (read-elves "resources/day23-sample-simple.txt"))

  (find-next-move simple-grid move-order [2 2])

  (scatter-elves {:grid simple-grid
                  :moves move-order}
                 nil)

  (reduce scatter-elves
          {:grid simple-grid
           :moves move-order}
          (range 2))
  ;
  )


;; part 1
(let [starting-grid (read-elves "resources/day23.txt")
      steps-to-take 10
      {grid :grid} (reduce scatter-elves
                           {:grid starting-grid
                            :moves move-order}
                           (range steps-to-take))
      min-x (->> grid (map first) (reduce min))
      max-x (->> grid (map first) (reduce max))
      min-y (->> grid (map second) (reduce min))
      max-y (->> grid (map second) (reduce max))]
  (println grid)
  (println min-x max-x min-y max-y)
  (- (* (inc (- max-x min-x)) (inc (- max-y min-y)))
     (count grid)))

;; part 2
(loop [grid (read-elves "resources/day23.txt")
       moves move-order
       i 1]
  (println i)
  (let [{next-grid :grid
         next-move-order :moves} (scatter-elves {:grid grid
                                                 :moves moves}
                                                nil)]
    (if (= grid next-grid)
      i
      (recur next-grid next-move-order (inc i)))))