(ns ryan.day24
  (:require [clojure.string :as str]))

(defn read-grid [file]
  (let [lines (->> file
                   slurp
                   str/split-lines
                   butlast
                   (drop 1)
                   vec)
        width (- (count (first lines)) 2)
        height (count lines)
        grid (reduce-kv (fn [acc y line]
                          (reduce-kv (fn [acc2 x c]
                                       (if (= c \.)
                                         acc2
                                         (conj acc2 [[x y] c])))
                                     acc
                                     (->> line butlast (drop 1) vec)))
                        []
                        lines)]
    {:grid grid
     :expedition [0 -1]
     :target [(dec width) height]
     :width width
     :height height}))

(read-grid "resources/day24-simple.txt")

(defn move-south [[x y] height]
  (if (= height (inc y))
    [x 0]
    [x (inc y)]))

(defn move-north [[x y] height]
  (if (zero? y)
    [x (dec height)]
    [x (dec y)]))

(defn move-east [[x y] width]
  (if (= width (inc x))
    [0 y]
    [(inc x) y]))

(defn move-west [[x y] width]
  (if (zero? x)
    [(dec width) y]
    [(dec x) y]))

;; memoize pattern with declare ahead (so the fn uses the memoized verison of itself)
;; hardcodes grid data for speed
(declare compute-grid)

(let [{:keys [grid width height target expedition]} (read-grid "resources/day24.txt")]
  (def starting-grid grid)
  (def starting-width width)
  (def starting-height height)
  (def expedition-target target)
  (def expedition-start expedition))

(defn advance-grid [grid]
  (reduce (fn [acc [p c]]
            (condp = c
              \# (conj acc [p \#])
              \v (conj acc [(move-south p starting-height) \v])
              \^ (conj acc [(move-north p starting-height) \^])
              \> (conj acc [(move-east p starting-width) \>])
              \< (conj acc [(move-west p starting-width) \<])))
          []
          grid))

(defn compute-grid-helper [steps]
  (if (zero? steps)
    starting-grid
    (advance-grid (compute-grid (dec steps)))))

(def compute-grid (memoize compute-grid-helper))

(defn gather-taken-spots [grid]
  (reduce (fn [acc [p]]
            (conj acc p))
          #{}
          grid))

(defn get-taken-spots-helper [steps]
  (gather-taken-spots (compute-grid steps)))

(def get-taken-spots (memoize get-taken-spots-helper))

;; bfs
;; (time
;;  (loop [[[[x y :as expedition] steps] & others] [[expedition-start 0]]]
;;    (let [blizzards (get-taken-spots steps)]
;;     ;; (Thread/sleep 100)
;;      (println x y steps (count others))
;;      (cond
;;        (= expedition expedition-target) steps
;;        (blizzards expedition) (recur others)
;;        (and (not= expedition expedition-start)
;;             (or (neg? x) (neg? y)
;;                 (= starting-width x) (= starting-height y))) (recur others)
;;        :else (recur (conj (vec others)
;;                           [[x y] (inc steps)]
;;                           [[(inc x) y] (inc steps)]
;;                           [[(dec x) y] (inc steps)]
;;                           [[x (inc y)] (inc steps)]
;;                           [[x (dec y)] (inc steps)]))))))

;; dfs
(def shortest-path (atom Integer/MAX_VALUE))
(def seen-space-time (atom #{}))

(defn taxi-distance [[p1x p1y] [p2x p2y]]
  (+ (abs (- p1x p2x)) (abs (- p1y p2y))))

;; part 1
(time
 (loop [[[[x y :as expedition] steps] & others] [[expedition-start 0]]
        print-check 0]
    ;; (Thread/sleep 100)
   (when (zero? (mod print-check 1000000))
     (println x y steps (count others)))
   (cond
     (nil? expedition) @shortest-path
     (@seen-space-time [expedition steps]) (recur others (inc print-check))
     (>= (+ (taxi-distance expedition expedition-target) steps) @shortest-path) (recur others (inc print-check))
    ;;  (>= steps @shortest-path) (recur others)
     (= expedition expedition-target) (do (swap! shortest-path min steps)
                                          (println "made it!" @shortest-path)
                                          (recur others (inc print-check)))
     ((get-taken-spots steps) expedition) (recur others (inc print-check))
     (and (not= expedition expedition-start)
          (or (neg? x) (neg? y)
              (= starting-width x) (= starting-height y))) (recur others (inc print-check))
     :else (do 
             (swap! seen-space-time conj [expedition steps])
             (recur (conj others
                            [[(dec x) y] (inc steps)]
                            [[x (dec y)] (inc steps)]
                            [[x y] (inc steps)]
                            [[x (inc y)] (inc steps)]
                            [[(inc x) y] (inc steps)])
                    (inc print-check))))))

(defn journey-to [start-pos target-pos starting-steps starting-max]
  (let [shortest-path (atom starting-max)
        seen-space-time (atom #{})]
    (time
     (loop [[[[x y :as expedition] steps] & others] [[start-pos starting-steps]]
            print-check 0]
    ;;    (Thread/sleep 100)
    ;;    (println x y steps (count others))

       (when (zero? (mod print-check 1000000))
         (println x y steps (count others)))
       (cond
         (nil? expedition) @shortest-path
         (@seen-space-time [expedition steps]) (recur others (inc print-check))
         (>= (+ (taxi-distance expedition target-pos) steps) @shortest-path) (recur others  (inc print-check))
    ;;  (>= steps @shortest-path) (recur others)
         (= expedition target-pos) (do (swap! shortest-path min steps)
                                       (println "made it!" @shortest-path)
                                       (recur others  (inc print-check)))
         ((get-taken-spots steps) expedition) (recur others  (inc print-check))
         (and (not= expedition start-pos)
              (or (neg? x) (neg? y)
                  (>= x starting-width) (>= y starting-height))) (recur others  (inc print-check))
         :else (do
                 (swap! seen-space-time conj [expedition steps])
                 (recur (conj others
                              [[(dec x) y] (inc steps)]
                              [[x (dec y)] (inc steps)]
                              [[x y] (inc steps)]
                              [[x (inc y)] (inc steps)]
                              [[(inc x) y] (inc steps)])
                         (inc print-check))))))))

(defn journey-back [start-pos target-pos starting-steps starting-max]
  (let [shortest-path (atom starting-max)
        seen-space-time (atom #{})]
    (time
     (loop [[[[x y :as expedition] steps] & others] [[start-pos starting-steps]]
            print-check 0]
    ;;    (Thread/sleep 100)
    ;;    (println x y steps (count others))

       (when (zero? (mod print-check 1000000))
         (println x y steps (count others)))
       (cond
         (nil? expedition) @shortest-path
         (@seen-space-time [expedition steps]) (recur others (inc print-check))
         (>= (+ (taxi-distance expedition target-pos) steps) @shortest-path) (recur others (inc print-check))
    ;;  (>= steps @shortest-path) (recur others)
         (= expedition target-pos) (do (swap! shortest-path min steps)
                                       (println "made it!" @shortest-path)
                                       (recur others (inc print-check)))
         ((get-taken-spots steps) expedition) (recur others (inc print-check))
         (and (not= expedition start-pos)
              (or (neg? x) (neg? y)
                  (>= x starting-width) (>= y starting-height))) (recur others (inc print-check))
         :else (do
                 (swap! seen-space-time conj [expedition steps])
                 (recur (conj others
                              [[(inc x) y] (inc steps)]
                              [[x (inc y)] (inc steps)]
                              [[x y] (inc steps)]
                              [[x (dec y)] (inc steps)]
                              [[(dec x) y] (inc steps)])
                        (inc print-check))))))))

(journey-to expedition-start expedition-target 0 500)
(journey-back expedition-target expedition-start 343 1000)
(journey-to expedition-start expedition-target 663 1500)
