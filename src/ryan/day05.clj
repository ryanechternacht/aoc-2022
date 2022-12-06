(ns ryan.day05
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.string :as s]))

(defn get-cols [piles]
  (range piles))

(defn build-stacks [lines piles]
  (let [cols (map #(inc (* 4 %)) (range piles))]
    (vec (for [c cols]
           (->> lines
                (map #(subs % c (inc c)))
                (remove str/blank?)
                (reduce conj '())
                vec)))))

(defn get-int-from-string [s i]
  (-> s
      (nth i)
      int
      (- (int \0))))

(defn build-moves [lines]
  (map (fn [line]
         (let [[_ boxes _ from _ to] (str/split line #" ")]
           {:boxes (Integer/parseInt boxes)
            :from (Integer/parseInt from)
            :to (Integer/parseInt to)}))
       lines))

(comment
  (build-stacks ["    [D]    "
                 "[N] [C]    "
                 "[Z] [M] [P]"]
                3)
  (get-int-from-string "move 1 from 2 to 1" 17) ; 5 12 17
  (build-moves ["move 1 from 2 to 1"
                "move 3 from 1 to 3"
                "move 12 from 2 to 14"
                "move 1 from 1 to 2"])
  ;
  )

(defn read-file [file depth piles]
  (let [lines (str/split-lines (slurp file))]
    {:stacks (build-stacks (take depth lines) piles)
     :moves (build-moves (drop (+ depth 2) lines))}))

(defn make-a-move [stacks {:keys [boxes to from]}]
  (reduce (fn [s _]
            (let [popped (last (nth s (dec from)))]
              (-> s
                  (update (dec from) pop)
                  (update (dec to) conj popped))))
          stacks
          (range boxes)))

(comment
  (make-a-move [["Z" "N"] ["M" "C" "D"] ["P"]]
               {:boxes 1, :from 2, :to 1})
  )

; part 1
(let [{:keys [stacks moves]}
      (read-file "resources/day05.txt" 8 9)]
      (->> (reduce make-a-move stacks moves)
           (map last)
           (str/join "")))

(defn make-a-move-2 [stacks {:keys [boxes to from]}]
  (let [popped (take-last boxes (nth stacks (dec from)))]
    (-> stacks
        (update (dec from) #(vec (drop-last boxes %)))
        (update (dec to) #(apply conj % popped)))))

(comment
  (make-a-move-2 [["Z" "N"] ["M" "C" "D"] ["P"]]
               {:boxes 3, :from 2, :to 1})
  
  )

; part 2
(let [{:keys [stacks moves]}
      (read-file "resources/day05.txt" 8 9)]
  (->> (reduce make-a-move-2 stacks moves)
       (map last)
       (str/join "")))