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

