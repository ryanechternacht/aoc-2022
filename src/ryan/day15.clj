(ns ryan.day15
  (:require [clojure.string :as str]))

(defn read-sensors [file]
  (->> file
       slurp
       (str/split-lines)
       (map #(re-seq #"[-\d]+" %))
       (map #(->> %
                  (map parse-long)
                  (partition 2)
                  (map vec)))))

(defn taxi-distance [[p1x p1y] [p2x p2y]]
  (+ (abs (- p1x p2x)) (abs (- p1y p2y))))

(defn get-taxi-points [[x y] d]
  (concat (map (fn [i] [(+ x i) (+ y d (- i))]) (range d)) ; quad 1
          (map (fn [i] [(+ x d (- i)) (+ y (- i))]) (range d)) ; quad 4
          (map (fn [i] [(- x i) (+ y (- d) i)]) (range d)) ; quad 3
          (map (fn [i] [(+ x (- d) i) (+ y i)]) (range d))))

(defn get-less-than-taxi [c e] ;center and edge
  (mapcat #(get-taxi-points c %) (range 1 (inc (taxi-distance c e)))))

(comment
  (taxi-distance [0 0] [2 1])

  (get-taxi-points [0 0] 3)
  (get-taxi-points [3 4] 3)

  (get-less-than-taxi [0 0] [1 2])
  (count (get-less-than-taxi [0 0] [1 2]))

  (get-less-than-taxi [3 4] [4 7])
  (count (get-less-than-taxi [3 4] [4 0]))
  ;
  )

(defn build-map [acc [sensor beacon]]
  (reduce (fn [a p] (if (a p) ;;if there's already something there 
                      a       ;; don't overwrite so we can keep the beacon locations
                      (assoc a p "#")))
          (-> acc
              (assoc sensor "S")
              (assoc beacon "B"))
          (get-less-than-taxi sensor beacon)))

(defn visualize-cave [cave]
  (let [points (->> cave (map first))
        min-x (->> points (map first) (reduce min))
        max-x (->> points (map first) (reduce max) inc)
        min-y (->> points (map second) (reduce min))
        max-y (->> points (map second) (reduce max) inc)]
    (map (fn [y]
           (map (fn [x]
                  (if (nil? (cave [x y]))
                    " "
                    (cave [x y])))
                (range min-x max-x)))
         (range min-y max-y))))

(comment
  (map println (visualize-cave (build-map {} [[0 0] [1 2]])))
  ;
  )

;; part 1 (failure)
(->> "resources/day15-sample.txt"
     read-sensors
     (reduce (fn [acc p]
               (println p)
               (build-map acc p))
             {})
    ;;  visualize-cave
    ;;  (map println))
     (filter (fn [[[_ y] _]]
               (= y 10)))
     (filter (fn [[_ c]]
               (not= c "B")))
     count)

(defn get-min-max-x [pairs]
  (reduce (fn [acc [[p1x :as p1] p2]]
            (let [tx (taxi-distance p1 p2)]
              (-> acc
                  (update :min min (- p1x tx))
                  (update :max max (+ p1x tx)))))
          {:min Integer/MAX_VALUE
           :max Integer/MIN_VALUE}
          pairs))

(defn is-within-taxi-distance? [p [p1 p2]]
  (<= (taxi-distance p p1) (taxi-distance p1 p2)))

(defn if-mapped? [pairs point]
  (cond
    (some (fn [[p _]]
            (= p point)) pairs)
    "S"

    (some (fn [[_ p]]
            (= p point)) pairs)
    "B"

    (some #(do
            ;;  (println "check" point %)
             (is-within-taxi-distance? point %)) pairs)
    "#"

    :else " "))

(comment
  (get-min-max-x [[[3 4] [-2 0]] [[8 0] [-2 -2]]])

  (is-within-taxi-distance? [0 2] [[3 3] [4 6]])

  (if-mapped? (read-sensors "resources/day15-sample.txt") [2 17])

  ;
  )

;; part 1
(let [sensors (read-sensors "resources/day15.txt")
      {min-x :min max-x :max} (get-min-max-x sensors)
      y 2000000
      row (map (fn [x] [x y]) (range min-x (inc max-x)))]
  (println min-x max-x)
  (->> row
       (map #(do (when (= 0 (mod (first %) 1000000))
                   (println %))
                 (if-mapped? sensors %)))
       (filter #(= % "#"))
       count))

(defn set-in-map [cave [px py] c]
  (if (and (< -1 py (count cave))
           (< -1 px (count (first cave))))
    (assoc-in cave [py px] c)
    cave))

(comment
  (set-in-map (vec (repeat 5 (vec (repeat 5 " "))))
              [4 4]
              "3")
  ;
  )

(defn build-map-2 [acc [s b]]
  (println [s b])
  (reduce (fn [a [px py :as p]]
            (if (not= " " (get-in a [py px])) ;;if there's already something there 
              a       ;; don't overwrite so we can keep the beacon locations
              (set-in-map a p "#")))
          (-> acc
              (set-in-map s "S")
              (set-in-map b "B"))
          (get-less-than-taxi s b)))


;; part 1 (again as a 2d array)
(let [sensors (read-sensors "resources/day15.txt")
      side 4000000
      initial-space (vec (repeat side (vec (repeat side " "))))]
  (->> sensors
       (reduce build-map-2
               initial-space)
       (flatten)
       (keep-indexed (fn [i x] (when (= x " ") i)))))


(defn read-sensors-2 [file]
  (->> file
       slurp
       (str/split-lines)
       (map #(re-seq #"[-\d]+" %))
       (map #(->> %
                  (map parse-long)
                  (partition 2)
                  (map vec)))
       (map (fn [[p1 p2]] [p1 p2 (taxi-distance p1 p2)]))))

(defn is-within-taxi-distance?-2 [p [p1 _ taxi]]
  (<= (taxi-distance p p1) taxi))

(defn if-mapped?-2 [pairs point]
  (cond
    (some (fn [[p _]]
            (= p point)) pairs)
    "S"

    (some (fn [[_ p]]
            (= p point)) pairs)
    "B"

    (some #(do
            ;;  (println "check" point %)
             (is-within-taxi-distance?-2 point %)) pairs)
    "#"

    :else " "))


;; part 1 again but optimized
(let [sensors (read-sensors-2 "resources/day15.txt")
      {min-x :min max-x :max} (get-min-max-x sensors)
      y 2000000
      row (map (fn [x] [x y]) (range min-x (inc max-x)))]
  (println min-x max-x)
  (->> row
       (map #(do (when (= 0 (mod (first %) 1000000))
                   (println %))
                 (if-mapped?-2 sensors %)))
       (filter #(= % "#"))
       count))


;; part 2
(def tracker (atom nil))
(def res (future
           (let [sides 4000000
                 points (for [x (range (inc sides)) y (range (inc sides))] [x y])
                 sensors (read-sensors-2 "resources/day15.txt")]
             (->> points
                  (map (fn [[_ y :as p]]
                         (when (= 0 (rem y 1000000))
                           (reset! tracker p))
                         (if-mapped?-2 sensors p)))
                  (keep-indexed (fn [i res] (when (= res " ") i)))
                  (take 1)))))

(defn generate-possibilities [sensors side-length]
  (let [is-on-board (fn [[px py]]
                      (and (<= 0 py side-length)
                           (<= 0 px side-length)))
        diagonals (mapcat (fn [[p _ taxi]]
                            (get-taxi-points p (inc taxi)))
                          sensors)]
    (reduce (fn [acc p]
              (cond-> acc
                (is-on-board p) (conj p)))
            #{}
            diagonals)))

;; part 2 (with hints from online)
;; my original approach built all the state up front which
;; wasn't performant, so I manually went 1 by 1 to find the gap
(let [sensors (read-sensors-2 "resources/day15.txt")
      adjusted-sensors (->> sensors (drop 6) (take 1))
      sides 4000000
      possibilities (generate-possibilities adjusted-sensors sides)
      _ (println "possibilities" (count possibilities))]
  (->> possibilities
       (pmap (fn [p] {:point p
                      :result (if-mapped?-2 sensors p)}))
       (filter #(= " " (:result %)))))

(+ (* 4000000 3138881) 3364986)