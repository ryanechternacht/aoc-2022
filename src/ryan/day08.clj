(ns ryan.day08
  (:require [clojure.string :as str]
            [clojure.test :as t]))

(defn read-trees [file]
  (->> file
       slurp
       (str/split-lines)
       (map vec)
       vec
       (reduce-kv (fn [acc y line]
                    (reduce-kv (fn [acc2 x t]
                                 (assoc acc2 [x y] {:height (Integer/parseInt (str t))
                                                    :visible 0
                                                    :scenic 1}))
                               acc
                               line))
                  {})))

;; line should be a coll of [x y] points
;; e.g. [0 0] [1 0] [2 0] for the first row of a 3x3
(defn set-visibility-for-line [trees line]
  (:ts (reduce (fn [{:keys [ts tallest] :as acc} p]
                 (let [{height :height :as t} (ts p)]
                   (if (<= height tallest)
                     acc
                     (-> acc
                         (assoc :tallest height) ; update tallest
                         (update-in [:ts p :visible] inc) ; update visible counter of this tree
                         ))))
               {:ts trees :tallest -1}
               line)))

(defn make-all-rows-and-cols [height width]
  (concat (map (fn [x] ;; rows left -> right
                 (map (fn [y]
                        [x y])
                      (range height)))
               (range width))
          (map (fn [x] ;; rows right -> left
                 (map (fn [y]
                        [x y])
                      (range (dec height) -1 -1)))
               (range (dec width) -1 -1))
          (map (fn [y] ;; cols top -> bottom
                 (map (fn [x]
                        [x y])
                      (range height)))
               (range width))
          (map (fn [y] ;; cols bottom -> top
                 (map (fn [x]
                        [x y])
                      (range (dec height) -1 -1)))
               (range (dec width) -1 -1))))

(comment
  (def sample (read-trees "resources/day08-sample.txt"))
  (set-visibility-for-line sample (for [x (range 5)] [x 0]))
  (set-visibility-for-line sample (for [y (range 5)] [0 y]))
  (set-visibility-for-line sample (for [y (range 5)] [3 y]))
  (make-all-rows-and-cols 5 5))

; part 1
(let [trees (read-trees "resources/day08.txt")
      width (inc (reduce max (->> trees keys (map first))))
      height (inc (reduce max (->> trees keys (map second))))
      trees-with-visbility
      (reduce set-visibility-for-line
              trees
              (make-all-rows-and-cols width height))]
  (count
   (filter (fn [[_ {v :visible}]] (> v 0))
           trees-with-visbility)))

(defn get-scenic-line [trees [start-x start-y :as start] [adjust-x adjust-y]]
  (let [starting-height (:height (trees start))]
    (loop [[px py] [(+ start-x adjust-x) (+ start-y adjust-y)]
           trees-seen 1]
      (let [{:keys [height]} (trees [px py])]
        (cond
          (nil? height) (dec trees-seen)
          (>= height starting-height) trees-seen
          :else (recur [(+ px adjust-x) (+ py adjust-y)] (inc trees-seen)))))))

(def get-scenic-left #(get-scenic-line %1 %2 [-1 0]))
(def get-scenic-right #(get-scenic-line %1 %2 [1 0]))
(def get-scenic-up #(get-scenic-line %1 %2 [0 -1]))
(def get-scenic-down #(get-scenic-line %1 %2 [0 1]))

(defn update-scenicness [trees p]
  (-> trees
      (update-in [p :scenic] * (get-scenic-left trees p))
      (update-in [p :scenic] * (get-scenic-right trees p))
      (update-in [p :scenic] * (get-scenic-up trees p))
      (update-in [p :scenic] * (get-scenic-down trees p))))

(comment
  (get-scenic-line sample [2 3] [-1 0])
  (get-scenic-line sample [4 3] [-1 0])
  (get-scenic-left sample [4 3])

  (get-scenic-left sample [2 3])
  (get-scenic-right sample [2 3])
  (get-scenic-up sample [2 3])
  (get-scenic-down sample [2 3])

  (update-scenicness sample [2 3])
  ;
  )

; part 2
(let [trees (read-trees "resources/day08.txt")
      width (inc (reduce max (->> trees keys (map first))))
      height (inc (reduce max (->> trees keys (map second))))
      all-cells (for [x (range width)
                      y (range height)]
                  [x y])
      trees-with-scenicness (reduce update-scenicness
                                    trees
                                    all-cells)]
  (->> trees-with-scenicness
       (map second)
       (map :scenic)
       (reduce max)))
