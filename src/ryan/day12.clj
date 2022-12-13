(ns ryan.day12
  (:require [clojure.string :as str]))

(defn read-map [file]
  (->> file
       slurp
       (str/split-lines)
       (map vec)
       vec
       (reduce-kv
        (fn [acc y line]
          (reduce-kv
           (fn [acc2 x c]
             (let [height (condp = c
                            \S 0
                            \E 26
                            (- (int c) (int \a)))]
               (cond-> acc2
                 true (assoc-in [:terrain [x y]] {:height height
                                                  :shortest (if (= c \S)
                                                              0
                                                              Integer/MAX_VALUE)})
                 (= c \S) (assoc :start [x y])
                 (= c \E) (assoc :end [x y]))))
           acc
           line))
        {:terrain {}
         :start nil
         :end nil})))

(defn can-reach [from to]
  (>= (inc from) to))

(defn get-neighbors [[x y] steps]
  [{:loc [(inc x) y]
    :prior [x y]
    :steps (inc steps)}
   {:loc [(dec x) y]
    :prior [x y]
    :steps (inc steps)}
   {:loc [x (inc y)]
    :prior [x y]
    :steps (inc steps)}
   {:loc [x (dec y)]
    :prior [x y]
    :steps (inc steps)}])

(defn get-to-all-points [{:keys [start end] :as original}]
  (loop [{:keys [terrain] :as acc} original
         [{:keys [loc steps prior]} & others] (get-neighbors start 0)]
    ;; (println "loop" loc (count others) steps)
    ;; (println acc)
    ;; (println loc steps prior)
    ;; (println others)
    ;; (Thread/sleep 10)
    (let [{:keys [height shortest] :as t} (terrain loc)
        ;;   _ (println "height shortest" height shortest)
          {prior-height :height :as prior-spot} (terrain prior)
        ;;   _ (println "prior-height" prior-height "prior-spot" prior-spot)
          ]
      (cond
        (nil? loc) (do (println "done") acc)
        (nil? t) (do #_(println "nil?") (recur acc others))
        (<= shortest steps) (do #_(println "prior") (recur acc others))

        (can-reach prior-height height)
        (let [updated (assoc-in acc [:terrain loc :shortest] steps)]
        ;;   (println "let")
        ;;   (if (= loc end)
            ;; (do #_(println "success!") updated)
          (do #_(println "keep going") (recur updated (concat others (get-neighbors loc steps))))) ;; todo
        :else (do #_(println "can't reach") (recur acc others))))))

(comment
  (can-reach 5 6)
  (get-neighbors [3 5] 4))

;; this solution is 2 too high (341 vs. 339 which is correct)
(->> "resources/day12.txt"
     read-map
     get-to-all-points
    ;;  ((fn [{:keys [terrain]}]
    ;;     (map-indexed (fn [y line]
    ;;                    (map-indexed (fn [x c]
    ;;                                   (:shortest (terrain [x y])))
    ;;                                 (range 66)))
    ;;                  (range 41))))
    ;;  (map println))
     ((fn [{:keys [end terrain]}]
        (:shortest (terrain end)))))

;; part 2 
;; i just spot checked my input (only 41 b's all in the 2nd column
;; so I just choose the best a, which is 7 closer than the start)