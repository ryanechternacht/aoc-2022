(ns ryan.day25
  (:require [clojure.string :as str]))

(defn read-snafu [file]
  (->> file
       slurp
       str/split-lines))

(def snafu-chars {\= -2
                  \- -1
                  \0 0
                  \1 1
                  \2 2})

(def digit-to-snafu (->> snafu-chars
                         (map (fn [[k v]] [v k]))
                         (into {})))

;; on a plane so can't look this up
(defn pow [x n]
  (reduce * 1 (repeat n x)))

(defn snafu-to-decimal [snafu]
  (->> snafu
       reverse
       (map-indexed (fn [i x] (* (snafu-chars x)
                                 (pow 5 i))))
       (reduce +)))

(comment
  (pow 5 1)
  (pow 5 2)
  (pow 5 0)

  (snafu-to-decimal "1")
  (snafu-to-decimal "2")
  (snafu-to-decimal "1=")
  (snafu-to-decimal "1-")
  (snafu-to-decimal "1=11-2") ;; 2022
  (snafu-to-decimal "1-0---0") ;; 12345
  (snafu-to-decimal "1121-1110-1=0") ;; 314159265
  ;
  )

(defn find-largest-digit [num]
  (reduce (fn [_ x]
            (when (zero? (quot num (* 2 (pow 5 x))))
              (reduced x)))
          (range)))

;; on a plane so can't look this up
(defn abs [x]
  (max x (- x)))

(defn find-correct-digit [num place]
  (reduce (fn [{:keys [r] :as acc} digit]
            (let [dist (- num (* digit (pow 5 place)))]
              (if (< (abs dist) (abs r))
                {:r dist
                 :d digit}
                acc)))
          {:r Integer/MAX_VALUE
           :d nil}
          (range -2 3)))

(defn decimal-to-snafu [num]
  (let [largest-digit (find-largest-digit num)
        s (:s (reduce (fn [{:keys [remaining s]} p]
                        (let [{:keys [r d]} (find-correct-digit remaining p)]
                          {:remaining r
                           :s (conj s (digit-to-snafu d))}))
                      {:remaining num
                       :s []}
                      (range largest-digit -1 -1)))]
    (str/join "" s)))

(comment
  (find-largest-digit 62)
  (find-correct-digit 22 2)
  (find-correct-digit -3 1)

  (snafu-to-decimal "1-2")
  (decimal-to-snafu 22)

  (snafu-to-decimal "222")
  (decimal-to-snafu 62)
  (find-largest-digit 62)


  (decimal-to-snafu 63)
  (snafu-to-decimal "1===")

  (snafu-to-decimal "201=")
  (decimal-to-snafu 253)

  (snafu-to-decimal "2222")
  (decimal-to-snafu 312)
  ; 
  )


(->> "resources/day25-sample.txt"
     read-snafu
     (map snafu-to-decimal)
     (reduce +)
     (decimal-to-snafu))