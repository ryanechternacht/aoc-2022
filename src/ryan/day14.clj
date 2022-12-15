(ns ryan.day14
  (:require [clojure.string :as str]))

(let [[px py] [5 1]
      [nx ny] [0 3]]
  [(+ px (compare nx px))
   (+ py (compare ny py))])

(defn read-rock-formation [line]
  (let [points (->> line
                    (#(str/split % #" -> "))
                    (map (fn [x]
                           (map parse-long (str/split x #","))))
  (map vec))]
    (:acc (reduce (fn [{:keys [prior-point acc]} [nx ny :as next-point]]
              {:prior-point next-point
               :acc (loop [[px py :as p] prior-point
                           acc2 acc]
                      (let [new-acc (conj acc2 p)]
                        (if (not= next-point p)
                          (recur [(+ px (compare nx px))
                                  (+ py (compare ny py))]
                                 new-acc)
                          new-acc)))})
            {:prior-point (first points)
             :acc []}
            (rest points)))))

(defn determine-boundaries [m]
  (let [points (map first m)]
    {:cave m
     :min-x (->> points (map first) (reduce min))
     :max-x (->> points (map first) (reduce max))
     :max-y (->> points (map second) (reduce max))}))

(defn build-cave [file]
  (->> file
       slurp
       str/split-lines
       (map read-rock-formation)
       (apply concat)
       (reduce (fn [acc p] (assoc acc p :rock)) {})
       determine-boundaries))

(defn is-off-map [{:keys [min-x max-x max-y]} [px py]]
  (or (> py max-y)
      (or (< px min-x) (> px max-x))))

(comment 
  (is-off-map {:min-x 490 :max-x 505 :max-y 8} [502 10])
  ;
  )

(defn find-next-sand-spot [{:keys [cave] :as state}]
  (loop [[px py :as p] [500 0]]
    (cond
      (is-off-map state p) :done

      (nil? (cave [px (inc py)])) (recur [px (inc py)])
      (nil? (cave [(dec px) (inc py)])) (recur [(dec px) (inc py)])
      (nil? (cave [(inc px) (inc py)])) (recur [(inc px) (inc py)])

      :else p)))

(defn visualize-cave [{:keys [cave max-y min-x max-x]}]
  (map (fn [y]
         (map (fn [x]
                ({:sand "o"
                  :rock "#"
                  nil " "} (cave [x y])))
              (range (- min-x 2) (+ max-x 3))))
       (range 0 (+ max-y 2))))

;; part 1
(let [cave (build-cave "resources/day14-sample.txt")
      sand-filled-cave
      (reduce (fn [c _] (let [outcome (find-next-sand-spot c)]
                          (if (= outcome :done)
                            (reduced c)
                            (assoc-in c [:cave outcome] :sand))))
              cave
              (range 1000))]
;;   (map println (visualize-cave sand-filled-cave)))

  (->> (:cave sand-filled-cave)
       (map second)
       (filter #(= % :sand))
       count))


(defn add-floor [{:keys [min-x max-x max-y] :as state}]
  (let [x-padding 200]
    (-> state
        (update :cave (fn [c]
                        (reduce (fn [acc p]
                                  (assoc acc p :rock))
                                c
                                (for [x (range (- min-x x-padding) (+ max-x x-padding))]
                                  [x (+ max-y 2)]))))
        (update :max-y + 2)
        (update :min-x - x-padding)
        (update :max-x + x-padding))))

(let [cave (build-cave "resources/day14.txt")
      cave-with-floor (add-floor cave)
      sand-filled-cave
      (reduce (fn [c _] (let [outcome (find-next-sand-spot c)]
                          (if (= outcome :done)
                            (reduced c)
                            (assoc-in c [:cave outcome] :sand))))
              cave-with-floor
              (range 100000))]
;;   (map println (visualize-cave sand-filled-cave)))

  (->> (:cave sand-filled-cave)
       (map second)
       (filter #(= % :sand))
       count))