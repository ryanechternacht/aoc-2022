(ns ryan.day21
  (:require [clojure.string :as str]))

(def ops {"+" + "-" - "*" * "/" /})

(defn read-monkeys [file]
  (->> file
       slurp
       str/split-lines
       (map #(str/split % #" "))
       (map (fn [[monkey left op right]]
              (let [m (subs monkey 0 4)]
                (if op
                  {:monkey m
                   :type :op
                   :left left
                   :op (ops op)
                   :right right}
                  {:monkey m
                   :type :num
                   :value (parse-long left)}))))))

;; part 1
(let [monkeys (read-monkeys "resources/day21.txt")
      solved (->> monkeys
                  (filter #(= :num (:type %)))
                  (map (fn [m] [(:monkey m) m]))
                  (into {}))
      unsolved (->> monkeys
                    (filter #(= :op (:type %)))
                    vec)
      final
      (loop [solved solved
             [{:keys [monkey left op right] :as m} & others] unsolved]
        (let [l (solved left)
              r (solved right)]
          (cond
            (nil? m) solved
            (and l r) (recur (assoc solved
                                    monkey
                                    {:monkey monkey
                                     :value (op (:value l) (:value r))
                                     :type :num})
                             others)
            :else (recur solved
                         (conj (vec others) m)))))]
  (final "root"))


(defn read-monkeys-2 [file]
  (->> file
       slurp
       str/split-lines
       (map #(str/split % #" "))
       (map (fn [[monkey left op right]]
              (let [m (subs monkey 0 4)]
                (cond
                  (= "root" m) {:monkey m
                                :type :op
                                :left left
                                :op "="
                                :right right}
                  (= "humn" m) {:monkey m
                                :type :human
                                :value (list (fn [x] x))}
                  op {:monkey m
                      :type :op
                      :left left
                      :op op
                      :right right}
                  :else {:monkey m
                         :type :num
                         :value (parse-long left)}))))))

(def inverse-op {"+" "-"
                 "-" "+"
                 "*" "/"
                 "/" "*"})

(let [monkeys (read-monkeys-2 "resources/day21.txt")
      solved (->> monkeys
                  (remove #(= :op (:type %)))
                  (map (fn [m] [(:monkey m) m]))
                  (into {}))
      unsolved (->> monkeys
                    (filter #(= :op (:type %)))
                    vec)
      final
      (loop [solved solved
             [{:keys [monkey left op right] :as m} & others] unsolved]
        (let [{l-type :type l-value :value} (solved left)
              {r-type :type r-value :value} (solved right)]
        ;;   (Thread/sleep 1000)
        ;;   (println monkey left op right)
        ;;   (println l-type l-value r-type r-value)
        ;;   (println (count solved) (count others))
        ;;   (println solved)
        ;;   (println)
          (cond
            (nil? m) solved
            (or (nil? l-type)
                (nil? r-type)) (recur solved
                                      (conj (vec others) m))
            (= monkey "root") ;; root
            (if (= :human l-type)
              (reduce (fn [acc f]
                        (f acc))
                      r-value
                      l-value)
              (reduce (fn [acc f]
                        (f acc))
                      l-value
                      r-value))
            ;;   (r-value l-value)
            ;;   (l-value r-value))

            (= :human l-type) (recur
                               (assoc solved
                                      monkey
                                      {:monkey monkey
                                       :type :human
                                       :value (conj l-value (fn [x] (str "(" (inverse-op op) " " x " " r-value ")")))})
                               others)
            (= :human r-type) (recur
                               (assoc solved
                                      monkey
                                      {:monkey monkey
                                       :type :human
                                       :value (if (#{"-" "/"} op)
                                                (conj r-value (fn [x] (str "(" op " " l-value " " x ")")))
                                                (conj r-value (fn [x] (str "(" (inverse-op op) " " x " " l-value ")"))))})
                               others)

            (and l-value r-value) (recur
                                   (assoc solved
                                          monkey
                                          {:monkey monkey
                                           :value ((ops op) l-value r-value)
                                           :type :num})
                                   others))))]
  final)

(+ (/ (- (* 150 4) 4) 2) 3)

(- (/ (+ (* (+ (* (- (/ (+ (* (- (/ (+ (- (* (- (/ (+ (* (- (/ (- (* (+ (/ (- (/ (+ (* (- (/ (+ (- (* (+ (* (- (/ (- (- (* (+ (/ (- (* (+ (/ (+ (/ (- (* (+ (- (- (- (* (- (/ (+ (/ (- (* (+ (/ (- (* (- 114076334160503 (/ (- (* 54426117311903 2) 724) 2)) 2) 580) 3) 864) 3) 296) 2) 190) 2) 892) 2) 959) 756) 288) 677) 4) 222) 2) 999) 2) 205) 2) 594) 4) 816) 3) 95) 139) 6) 589) 2) 21) 2) 533) 343) 2) 131) 4) 524) 2) 118) 2) 270) 2) 778) 4) 582) 5) 387) 4) 992) 7) 506) 821) 21) 190) 11) 413) 115) 645) 9) 529) 5) 323) 8) 981)
