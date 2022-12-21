(ns ryan.day20
  (:require [clojure.string :as str]))

(defn read-file [file]
  (->> file
       slurp
       str/split-lines
       (map-indexed (fn [i s] {:index i
                               :num (parse-long s)}))
       vec))

(defn get-by-original-index [coll idx]
  (first (keep-indexed (fn [i {index :index}] (when (= index idx) i)) coll)))

(defn remove-at [v i]
  (concat (take i v) (drop (inc i) v)))

(defn insert-at-circle [v i item]
  (let [count-v (count v)
        j (+ count-v i)
        j (cond (and (neg? i) (zero? j)) count-v
                (neg? j) (+ count-v (rem j count-v))
                :else (rem j count-v))]
    (concat (take j v) [item] (drop j v))))

(defn adjust-position [coll {:keys [index num] :as item}]
  (println index)
  (let [i (get-by-original-index coll index)]
    (-> coll
        (remove-at i)
        (insert-at-circle (+ i num) item))))

(comment
  (get-by-original-index (read-file "resources/day20-sample.txt") 1)
  (get-by-original-index [{:index 5, :num 0} {:index 6, :num 4}] 6)

  (remove-at [1 2 3 4 5 6 7 8 9] 2)

  (insert-at-circle [1 2 3 4 5 6 7 8 9] 2 "a")
  (insert-at-circle [1 2 3 4 5 6 7 8 9] 11 "a")
  (insert-at-circle [1 2 3 4 5 6 7 8 9] -2 "a")

  (adjust-position [{:index 0, :num 1} {:index 1, :num 1} {:index 2, :num -3}]
                   {:index 1, :num -1})
  
  (adjust-position [{:index 1, :num 1623178306} {:index 5, :num 0} {:index 3, :num 2434767459} {:index 6, :num 3246356612} {:index 2, :num -2434767459} {:index 0, :num 811589153} {:index 4, :num -1623178306}]
                   {:index 2, :num -2434767459})
  ;
  )

;; part 1
(let [original (read-file "resources/day20-sample.txt")
      adjusted (reduce adjust-position
                       original
                       original)
                    ;;    (take 7 original))
      nums (map :num adjusted)
      zero-idx (first
                (keep-indexed  #(when (zero? %2) %1) nums))
      cycled (cycle nums)
      positions ((juxt #(first (drop (+ zero-idx 1000) %))
                       #(first (drop (+ zero-idx 2000) %))
                       #(first (drop (+ zero-idx 3000) %)))
                 cycled)]
  (reduce + positions))


(defn read-file-2 [file]
  (->> file
       slurp
       str/split-lines
       (map-indexed (fn [i s] {:index i
                               :num (* 811589153 (parse-long s))}))
       vec))

(let [original (read-file-2 "resources/day20.txt")
      adjusted (reduce adjust-position
                       original
                       (take (* 10 (count original)) (cycle original)))
                    ;;    (take 7 original))
      nums (map :num adjusted)
      zero-idx (first
                (keep-indexed  #(when (zero? %2) %1) nums))
      cycled (cycle nums)
      positions ((juxt #(first (drop (+ zero-idx 1000) %))
                       #(first (drop (+ zero-idx 2000) %))
                       #(first (drop (+ zero-idx 3000) %)))
                 cycled)]
  (reduce + positions))