(ns ryan.day18
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn read-points [file]
  (->> file
       slurp
       str/split-lines
       (map #(vec (map parse-long (re-seq #"\d+" %))))))

(defn get-neighbors [[x y z]]
  #{[(inc x) y z]
    [(dec x) y z]
    [x (inc y) z]
    [x (dec y) z]
    [x y (inc z)]
    [x y (dec z)]})

(defn add-block [{:keys [sa blocks]} p]
  (let [neighbors (count (set/intersection blocks (get-neighbors p)))]
    {:blocks (conj blocks p)
     :sa (+ sa 6 (- (* neighbors 2)))}))

(comment
  (get-neighbors [1 2 3])
  (add-block {:sa 3
              :blocks #{[1 0 0]}}
             [0 0 0])
  ;
  )

;; part 1
(->> "resources/day18.txt"
     read-points
     (reduce add-block {:sa 0
                        :blocks #{}})
     :sa)

;; part 2
(->> "resources/day18.txt"
     read-points
     (map second)
     (reduce max))

;; x 1 - 3
;; y 1 - 3
;; z 1 - 6
;; 1,1,1 on the exterior

;; x 0 - 21
;; y 0 - 21
;; z 0 - 21
;; 1,1,1 on the exterior

(defn flood-fill [x-min x-max y-min y-max z-min z-max blocks start]
  (loop [exterior-blocks #{start}
         [[x y z :as p] & others] (vec (get-neighbors start))]
    (cond
      (nil? x) exterior-blocks

      (or (not (<= x-min x x-max))
          (not (<= y-min y y-max))
          (not (<= z-min z z-max))) (recur exterior-blocks others)

      (blocks p) (recur exterior-blocks others)

      (exterior-blocks p) (recur exterior-blocks others)

      :else (recur (conj exterior-blocks p)
                   (concat others (get-neighbors p))))))

;; sample
(let [start [1 1 1]
    ;;   [min-x max-x min-y max-y min-z max-z] [0 4 0 4 0 7]
      [min-x max-x min-y max-y min-z max-z] [-1 22 -1 22 -1 22]
      points (set (read-points "resources/day18.txt"))
      outside-points (flood-fill min-x max-x min-y max-y min-z max-z points start)

      {external-surface-area :sa} (reduce add-block
                                          {:sa 0
                                           :blocks #{}}
                                          outside-points)
      length (- (inc max-x) min-x)
      width (- (inc max-y) min-y)
      height (- (inc max-z) min-z)]
  (- external-surface-area
     (* 2 length width)
     (* 2 length height)
     (* 2 width height)))