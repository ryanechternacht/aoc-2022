(ns ryan.day22
  (:require [clojure.string :as str]))

(defn read-input [file]
  (let [lines (->> file
                   slurp
                   str/split-lines)
        grid (reduce-kv (fn [acc y line]
                          (reduce-kv (fn [acc2 x c]
                                       (condp = c
                                         \space acc2
                                         \. (assoc acc2 [x y] :space)
                                         \# (assoc acc2 [x y] :rock)))
                                     acc
                                     (vec line)))
                        {}
                        (vec (drop-last 2 lines)))]
    {:grid grid
     :instructions (mapcat #(if (parse-long %)
                              (repeat (parse-long %) \F)
                              %)
                           (re-seq #"[RL]|\d+" (last lines)))}))

(defn find-next-north [grid [px py]]
  (let [north-p [px (dec py)]]
    (if (grid north-p)
      [north-p (grid north-p)]
      (->> grid
           (filter (fn [[[x _] _]] (= px x)))
           (sort-by (fn [[[_ y] _]] y))
           reverse
           first))))

(defn find-next-south [grid [px py]]
  (let [south-p [px (inc py)]]
    (if (grid south-p)
      [south-p (grid south-p)]
      (->> grid
           (filter (fn [[[x _] _]] (= px x)))
           (sort-by (fn [[[_ y] _]] y))
           first))))

(defn find-next-east [grid [px py]]
  (let [east-p [(inc px) py]]
    (if (grid east-p)
      [east-p (grid east-p)]
      (->> grid
           (filter (fn [[[_ y] _]] (= py y)))
           (sort-by (fn [[[x _] _]] x))
           first))))

(defn find-next-west [grid [px py]]
  (let [west-p [(dec px) py]]
    (if (grid west-p)
      [west-p (grid west-p)]
      (->> grid
           (filter (fn [[[_ y] _]] (= py y)))
           (sort-by (fn [[[x _] _]] x))
           reverse
           first))))

(comment
  (def sample-grid (:grid (read-input "resources/day22-sample.txt")))
  (find-next-north sample-grid [7 4])
  (find-next-south sample-grid [7 5])
  (find-next-east sample-grid [10 0])
  (find-next-west sample-grid [8 0])

  (def real-grid (:grid (read-input "resources/day22.txt")))
  (find-next-north real-grid [23 109])
  
  ;
  )

(def turn-left {\E \N
                \N \W
                \W \S
                \S \E})

(def turn-right {\E \S
                 \S \W
                 \W \N
                 \N \E})

(def facing-value {\E 0
                   \S 1
                   \W 2
                   \N 3})

;; part 1
(let [{:keys [grid instructions]} (read-input "resources/day22.txt")
      starting-position (first (find-next-east grid [0 0])) ;; hack!

      {:keys [position facing]}
      (reduce (fn [{:keys [facing position] :as acc} instr]
                ;; (println position facing)
                (condp = instr
                  \R (update acc :facing turn-right)
                  \L (update acc :facing turn-left)
                  \F (let [[next-pos loc] (condp = facing
                                            \E (find-next-east grid position)
                                            \W (find-next-west grid position)
                                            \N (find-next-north grid position)
                                            \S (find-next-south grid position))]
                       (if (= loc :rock)
                         acc
                         (assoc acc :position next-pos)))))
              {:position starting-position
               :facing \E}
              instructions)]
  (+ (* 1000 (inc (second position)))
     (* 4 (inc (first position)))
     (facing-value facing)))


;; part 2
;;   1
;; 234
;;   56
(def sample-adjancies {;; 1 left to 3 top
                       [[8 0] \W] [[4 4] \S]
                       [[8 1] \W] [[5 4] \S]
                       [[8 2] \W] [[6 4] \S]
                       [[8 3] \W] [[7 4] \S]
                       [[4 4] \N] [[8 0] \E]
                       [[5 4] \N] [[8 1] \E]
                       [[6 4] \N] [[8 2] \E]
                       [[7 4] \N] [[8 3] \E]

                       ;; 1 up to 2 up
                       [[0 4] \N] [[11 0] \S]
                       [[1 4] \N] [[10 0] \S]
                       [[2 4] \N] [[9 0] \S]
                       [[3 4] \N] [[8 0] \S]
                       [[11 0] \N] [[0 4] \S]
                       [[10 0] \N] [[1 4] \S]
                       [[9 0] \N] [[2 4] \S]
                       [[8 0] \N] [[3 4] \S]

                       ;; 1 right to 6 right
                       [[11 0] \E] [[15 11] \W]
                       [[11 1] \E] [[15 10] \W]
                       [[11 2] \E] [[15 9] \W]
                       [[11 3] \E] [[15 8] \W]
                       [[15 11] \E] [[11 0] \W]
                       [[15 10] \E] [[11 1] \W]
                       [[15 9] \E] [[11 2] \W]
                       [[15 8] \E] [[11 3] \W]

                       ;; 2 left to 6 down
                       [[0 7] \W] [[12 11] \N]
                       [[0 6] \W] [[13 11] \N]
                       [[0 5] \W] [[14 11] \N]
                       [[0 4] \W] [[15 11] \N]
                       [[12 11] \S] [[0 7] \E]
                       [[13 11] \S] [[0 6] \E]
                       [[14 11] \S] [[0 5] \E]
                       [[15 11] \S] [[0 4] \E]

                       ;; 2 down to 5 down
                       [[0 7] \S] [[11 11] \N]
                       [[1 7] \S] [[10 11] \N]
                       [[2 7] \S] [[9 11] \N]
                       [[3 7] \S] [[8 11] \N]
                       [[11 11] \S] [[0 7] \N]
                       [[10 11] \S] [[1 7] \N]
                       [[9 11] \S] [[2 7] \N]
                       [[8 11] \S]  [[3 7] \N]

                       ;; 3 down to 5 left
                       [[7 7] \S] [[8 8] \E]
                       [[6 7] \S] [[8 9] \E]
                       [[5 7] \S] [[8 10] \E]
                       [[4 7] \S] [[8 11] \E]
                       [[8 8] \W] [[7 7] \N]
                       [[8 9] \W] [[6 7] \N]
                       [[8 10] \W] [[5 7] \N]
                       [[8 11] \W] [[4 7] \N]

                       ;; 4 right to 6 top
                       [[11 4] \E] [[15 8] \S]
                       [[11 5] \E] [[14 8] \S]
                       [[11 6] \E] [[13 8] \S]
                       [[11 7] \E] [[12 8] \S]
                       [[15 8] \N] [[11 4] \W]
                       [[14 8] \N] [[11 5] \W]
                       [[13 8] \N] [[11 6] \W]
                       [[12 8] \N] [[11 7] \W]})

;;  12
;;  3
;; 45
;; 6
(def adjancies (merge
                ;; 1 top to 6 left
                (zipmap (map (fn [x] [[0 (- 199 x)] \W]) (range 50))
                        (map (fn [x] [[(- 99 x) 0] \S]) (range 50)))
                (zipmap (map (fn [x] [[(- 99 x) 0] \N]) (range 50))
                        (map (fn [x] [[0 (- 199 x)] \E]) (range 50)))

                ;; 1 left to 4 left
                (zipmap (map (fn [x] [[50 (+ 0 x)] \W]) (range 50))
                        (map (fn [x] [[0 (- 149 x)] \E]) (range 50)))
                (zipmap (map (fn [x] [[0 (- 149 x)] \W]) (range 50))
                        (map (fn [x] [[50 (+ 0 x)] \E]) (range 50)))

                ;; 2 top to 6 down
                (zipmap (map (fn [x] [[(- 149 x) 0] \N]) (range 50))
                        (map (fn [x] [[(- 49 x) 199] \N]) (range 50)))
                (zipmap (map (fn [x] [[(- 49 x) 199] \S]) (range 50))
                        (map (fn [x] [[(- 149 x) 0] \S]) (range 50)))

                ;; 2 right to 5 right
                (zipmap (map (fn [x] [[149 (+ 0 x)] \E]) (range 50))
                        (map (fn [x] [[99 (- 149 x)] \W]) (range 50)))
                (zipmap (map (fn [x] [[99 (- 149 x)] \E]) (range 50))
                        (map (fn [x] [[149 (+ 0 x)] \W]) (range 50)))

                ;; 2 down to 3 right
                (zipmap (map (fn [x] [[(+ 100 x) 49] \S]) (range 50))
                        (map (fn [x] [[99 (+ 50 x)] \W]) (range 50)))
                (zipmap (map (fn [x] [[99 (+ 50 x)] \E]) (range 50))
                        (map (fn [x] [[(+ 100 x) 49] \N]) (range 50)))

                ;; 3 left to 4 top
                (zipmap (map (fn [x] [[50 (+ 50 x)] \W]) (range 50))
                        (map (fn [x] [[(+ 0 x) 100] \S]) (range 50)))
                (zipmap (map (fn [x] [[(+ 0 x) 100] \N]) (range 50))
                        (map (fn [x] [[50 (+ 50 x)] \E]) (range 50)))

                ;; 5 down and 6 right
                (zipmap (map (fn [x] [[(+ 50 x) 149] \S]) (range 50))
                        (map (fn [x] [[49 (+ 150 x)] \W]) (range 50)))
                (zipmap (map (fn [x] [[49 (+ 150 x)] \E]) (range 50))
                        (map (fn [x] [[(+ 50 x) 149] \N]) (range 50)))))

(let [{:keys [grid instructions]} (read-input "resources/day22.txt")
      starting-position (first (find-next-east grid [0 0])) ;; hack!

      {:keys [position facing]}
      (reduce (fn [{facing :facing [x y] :position :as acc} instr]
                (condp = instr
                  \R (update acc :facing turn-right)
                  \L (update acc :facing turn-left)
                  \F (let [[next-pos next-facing] (condp = facing
                                                           \E [[(inc x) y] \E]
                                                           \W [[(dec x) y] \W]
                                                           \N [[x (dec y)] \N]
                                                           \S [[x (inc y)] \S])
                           [next-pos next-facing] (if (nil? (grid next-pos))
                                                    (adjancies [[x y] facing])
                                                    [next-pos next-facing])]
                       (if (= (grid next-pos) :rock)
                         acc
                         (-> acc
                             (assoc :position next-pos)
                             (assoc :facing next-facing))))))
              {:position starting-position
               :facing \E}
              instructions)]
  (+ (* 1000 (inc (second position)))
     (* 4 (inc (first position)))
     (facing-value facing)))