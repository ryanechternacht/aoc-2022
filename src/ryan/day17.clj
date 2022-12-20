(ns ryan.day17
  (:require [clojure.string :as str]))

(defn make-horizontal-line [[px py]]
  [[px py]
   [(inc px) py]
   [(+ 2 px) py]
   [(+ 3 px) py]])

(defn make-plus [[px py]]
  [[(inc px) py]
   [px (inc py)]
   [(inc px) (inc py)]
   [(+ 2 px) (inc py)]
   [(inc px) (+ 2 py)]])

(defn make-angle [[px py]]
  [[px py]
   [(inc px) py]
   [(+ 2 px) py]
   [(+ 2 px) (inc py)]
   [(+ 2 px) (+ 2 py)]])

(defn make-vertical-line [[px py]]
  [[px py]
   [px (inc py)]
   [px (+ 2 py)]
   [px (+ 3 py)]])

(defn make-square [[px py]]
  [[px py]
   [(inc px) py]
   [px (inc py)]
   [(inc px) (inc py)]])

(def shapes (cycle [make-horizontal-line
                    make-plus
                    make-angle
                    make-vertical-line
                    make-square]))

(comment
  (make-horizontal-line [3 4])
  (make-plus [3 4])
  (make-angle [3 4])
  (make-vertical-line [3 4])
  (make-square [3 4])
  ;
  )

(def wind (->> "resources/day17-sample.txt"
               slurp
               str/trim-newline
               (map (fn [x] (fn [[px py]]
                              [(({\> inc \< dec} x) px) py])))
               cycle))

(defn is-illegal-position? [column [px py :as p]]
  (or (> px 6)
      (< px 0)
      (column p)
      (< py 0)))

(defn drop-one [shape]
  (map #(update % 1 dec) shape))

(def shape-count 5)
(def wind-count 40)

(defn rock-fall [{:keys [wind-index column]} i]
;;   (when (= 0 (rem i shape-count) (rem wind-index wind-count))
;;     (println "found loop" i (reduce max -1 (map second column))))
  (let [start-height (+ 4 (reduce max -1 (map second column)))]
    ;; (when (zero? (rem i 100))
    (println i (count column) (rem i shape-count) (rem wind-index wind-count) start-height) ;; start-height (count column)))
    (loop [shape ((->> shapes (drop i) first) [2 start-height])
           wind-index wind-index
           column column]
    ;;   (println "loop")
    ;;   (println shape)
    ;;   (println wind-index)
    ;;   (println column)
    ;;   (Thread/sleep 1000)
      (let [wind (->> wind (drop wind-index) first)
            potential-wind-shape (map wind shape)
            wind-shape (if (some #(is-illegal-position? column %) potential-wind-shape)
                         shape
                         potential-wind-shape)
            potential-drop (drop-one wind-shape)
            ;; _ (println potential-drop)
            ]
        (if (some #(is-illegal-position? column %) potential-drop)
          {:wind-index (inc wind-index)
        ;;    :column (set (filter (fn [[_ py]] (> py (- start-height 10))) (apply conj column wind-shape)))}
           :column (apply conj column wind-shape)}
          (recur potential-drop (inc wind-index) column))))))

(->> (reduce rock-fall
             {:wind-index 0
              :column #{}
              :seen-setups #{}}
             (range 400))
     :column
     (map second)
     (reduce max)
     inc)
 
;; 100 -> 800 (so every 700) repeats adding 1060 every time 
;; so we can figure out 0 -> (mod target 700) and then add
;; that to ((quot num 700) * 1060)

(- 1220 160)
(- 2280 1220)

(mod 1000000000000 700)
(quot 1000000000000 700)

(+ 608 (* 1428571428 1060)) ;; success!

;; for 
(* 8 4400)

1000000000000

(defn is-cycle? [coll search]
  (let [block-1 (take search coll)
        block-2 (take search (drop search coll))]
    (println block-1)
    (println block-2)
    (= block-1 block-2)))

;; find the cycle
(def diffs (->> "resources/day17-diffs.txt"
                slurp
                str/split-lines
                (map parse-long)
                (#(str/join "" %))))

22003032122401332212320132201330212300032201324 ;; cycle

603 ;; first time
2348 ;; 2nd time

(- 2348 603)
1745

2783 ;; height gained (from excel)

(quot 1000000000000 1745)
573065902

(mod 1000000000000 1745)
1010

(+ 1616 (* 2783 573065902))
1594842406882