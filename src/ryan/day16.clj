(ns ryan.day16
  (:require [clojure.string :as str]))

(defn read-valves [file]
  (->> file
       slurp
       (str/split-lines)
       (map #(str/split % #" "))
       (reduce (fn [acc [_ v _ _ rate _ _ _ _ & others]]
                 (let [r (parse-long (first (re-seq #"\d+" rate)))]
                   (assoc acc v {:rate r
                                 :to (flatten (map #(re-seq #"\w\w" %) others))})))
               {})))

(defn dijkstra-valves [valves start]
  (loop [acc {start 0}

         [{:keys [valve :steps]} & others]
         (map (fn [x] {:valve x
                       :steps 1}) (:to (valves start)))]
    (cond
      (nil? valve) acc
      (>= steps (get acc valve Integer/MAX_VALUE)) (recur acc others)

      :else
      (recur (assoc acc valve (inc steps))
             (concat others
                     (map (fn [x] {:valve x
                                   :steps (inc steps)})
                          (:to (valves valve))))))))

(defn condense-valves [valves]
  (let [principals (filter (fn [[k {:keys [rate]}]]
                             (or (pos-int? rate)
                                 (= k "AA")))
                           valves)]
    (reduce (fn [acc [k {:keys [rate]}]]
              (let [dijkstra (dijkstra-valves valves k)]
                (assoc acc k {:rate rate
                              :to (map (fn [v]
                                         [v (dijkstra v)])
                                       (map first principals))})))
            {}
            principals)))

(defn flood-for-releases [valves]
  (loop [done {:pressure 0}
         [{:keys [current path minutes pressure] :as p} & others]
         [{:current "AA" :path ["AA"] :minutes 30 :pressure 0}]]
    (if (nil? p)
      done
      (let [path-set (set path)
            to-visit (remove (fn [[v d]]
                               (or (path-set v)
                                   (< (- minutes d) 0)))
                             (:to (valves current)))]
        (if (seq to-visit)
          (recur done
                 ;; this ordering gives us depth first
                 ;; which is necessary, since breadth first stack overflows
                 (concat (map (fn [[v d]]
                                (let [min (- minutes d)
                                      rate (:rate (valves v))]
                                  {:current v
                                   :path (conj path v)
                                   :minutes min
                                   :pressure (+ pressure (* rate min))}))
                              to-visit)
                         others))
          (recur (if (> pressure (:pressure done))
                   p
                   done)
                 others))))))

;; part 1
(->> "resources/day16.txt"
     read-valves
     condense-valves
     flood-for-releases)


(defn flood-for-releases-2 [valves]
  (loop [done {:pressure 0}
         [{:keys [current-me minutes-me
                  current-elph minutes-elph
                  path  pressure] :as p} & others]
         [{:current-me "AA" :minutes-me 26
           :current-elph "AA" :minutes-elph 26
           :path ["AA"] :pressure 0}]
         i 0]
    ;; (println "loop")
    ;; (println "done" done)
    ;; (println "p" p)
    ;; (println "others" others)
    ;; (Thread/sleep 1000)
    ;; (println (count others) minutes)
    
    (when (zero? (mod i 100000))
      (println i (count others) (:pressure done)))
    
    (cond
      (nil? p) done
      (= 0 minutes-elph minutes-me) (do
                                      (recur
                                       (if (> pressure (:pressure done))
                                         p
                                         done)
                                       others
                                       (inc i)))
      ;; process me
      (>= minutes-me minutes-elph)
      (let [path-set (set path)
            to-visit (remove (fn [[v d]]
                               (or (path-set v)
                                   (< (- minutes-me d) 0)))
                             (:to (valves current-me)))]
            ;; _ (println "to-visit" to-visit)
            ;; _ (println "mapfn" (conj others
            ;;                          (map (fn [[v d]]
            ;;                                 (let [min (- minutes d)
            ;;                                       rate (:rate (valves v))]
            ;;                                   {:current v
            ;;                                    :path (conj path v)
            ;;                                    :minutes min
            ;;                                    :pressure (+ pressure (* rate min))}))
            ;;                               to-visit)))

        (if (seq to-visit)
          (recur done
                 ;; this ordering gives us depth first
                 ;; which is necessary, since breadth first stack overflows
                 (concat (map (fn [[v d]]
                                (let [min (- minutes-me d)
                                      rate (:rate (valves v))]
                                  {:current-me v
                                   :minutes-me min
                                   :current-elph current-elph
                                   :minutes-elph minutes-elph
                                   :path (conj path v)
                                   :pressure (+ pressure (* rate min))}))
                              to-visit)
                         others)
                 (inc i))
          (do
            ;; (println "p" p)
            ;; (println "to-visit" to-visit)
            ;; (println "valves" (:to (valves current)))
            ;; (println "clears-time-check" (remove (fn [[v d]]
            ;;                                        (< (- minutes d) 0))
            ;;                                      (:to (valves current))))
            ;; (println "clears-path-check" (remove (fn [[v d]]
            ;;                                        (path-set v))
            ;;                                      (:to (valves current))))
            (recur done
                   (conj others
                         (assoc p :minutes-me 0))
                   (inc i)))))

      :else
      ;; process elephant
      (let [path-set (set path)
            to-visit (remove (fn [[v d]]
                               (or (path-set v)
                                   (< (- minutes-elph d) 0)))
                             (:to (valves current-elph)))]
            ;; _ (println "to-visit" to-visit)
            ;; _ (println "mapfn" (conj others
            ;;                          (map (fn [[v d]]
            ;;                                 (let [min (- minutes d)
            ;;                                       rate (:rate (valves v))]
            ;;                                   {:current v
            ;;                                    :path (conj path v)
            ;;                                    :minutes min
            ;;                                    :pressure (+ pressure (* rate min))}))
            ;;                               to-visit)))

        (if (seq to-visit)
          (recur done
                 ;; this ordering gives us depth first
                 ;; which is necessary, since breadth first stack overflows
                 (concat (map (fn [[v d]]
                                (let [min (- minutes-elph d)
                                      rate (:rate (valves v))]
                                  {:current-elph v
                                   :minutes-elph min
                                   :current-me current-me
                                   :minutes-me minutes-me
                                   :path (conj path v)
                                   :pressure (+ pressure (* rate min))}))
                              to-visit)
                         others)
                 (inc i))
          (do
            ;; (println "p" p)
            ;; (println "to-visit" to-visit)
            ;; (println "valves" (:to (valves current)))
            ;; (println "clears-time-check" (remove (fn [[v d]]
            ;;                                        (< (- minutes d) 0))
            ;;                                      (:to (valves current))))
            ;; (println "clears-path-check" (remove (fn [[v d]]
            ;;                                        (path-set v))
            ;;                                      (:to (valves current))))
            (recur done
                   (conj others
                         (assoc p :minutes-elph 0))
                   (inc i))))))))

;; part 2
(->> "resources/day16.txt"
     read-valves
     condense-valves
     flood-for-releases-2)

;; TODO when one side runs out of minutes, we need to set it
;; to 0 (or nil or something) and then process the other side.
;; once both sides are out, then we mark it done