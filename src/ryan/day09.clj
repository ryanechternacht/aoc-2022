(ns ryan.day09
  (:require [clojure.string :as str]))

(defn read-steps [file]
  (->> file
       slurp
       str/split-lines
       (map #(str/split % #" "))
       (mapcat (fn [[l c]]
                 (repeat (Integer/parseInt c) l)))))

(defn clamp [n]
  (cond 
    (zero? n) 0
    (> n 0) 1
    (< n 0) -1))

(defn new-tail-pos [[phx phy] [ptx pty]]
  (let [diff-x (- phx ptx)
        diff-y (- phy pty)]
    (if (and (<= (abs diff-x) 1) (<= (abs diff-y) 1))
      [ptx pty]
      [(+ ptx (clamp diff-x)) (+ pty (clamp diff-y))])))

(defn move-head [{[phx phy] :ph pt :pt visited :visited} dir]
  (let [ph-new (condp = dir
                 "R" [(inc phx) phy]
                 "L" [(dec phx) phy]
                 "U" [phx (inc phy)]
                 "D" [phx (dec phy)])
        pt-new (new-tail-pos ph-new pt)]
    {:ph ph-new
     :pt pt-new
     :visited (conj visited pt-new)}))

(comment
  (new-tail-pos [1 1] [1 1])
  (new-tail-pos [1 2] [1 1])
  (new-tail-pos [1 0] [1 1])
  (new-tail-pos [0 1] [1 1])
  (new-tail-pos [1 3] [1 1])
  (new-tail-pos [3 1] [1 1])
  (new-tail-pos [3 2] [1 1])
  (new-tail-pos [2 3] [1 1])
  
  (move-head {:ph [0 0]
              :pt [0 0]
              :visited #{[0 0]}}
             "R")
  ;
  )
; part 1
(->> "resources/day09.txt"
     read-steps
     (reduce move-head
             {:ph [0 0]
              :pt [0 0]
              :visited #{[0 0]}})
     :visited
     count)

;; (defn move-head [{[phx phy] :ph pt :pt visited :visited} dir]
;;   (let [ph-new (condp = dir
;;                  "R" [(inc phx) phy]
;;                  "L" [(dec phx) phy]
;;                  "U" [phx (inc phy)]
;;                  "D" [phx (dec phy)])
;;         pt-new (new-tail-pos ph-new pt)]
;;     {:ph ph-new
;;      :pt pt-new
;;      :visited (conj visited pt-new)}))

(defn move-rope [{:keys [pieces visited]} dir]
  (let [[[phx phy] & tail] pieces
        head-new (condp = dir
                   "R" [(inc phx) phy]
                   "L" [(dec phx) phy]
                   "U" [phx (inc phy)]
                   "D" [phx (dec phy)])
        {:keys [new-body]} (reduce (fn [{:keys [new-body prior-piece]} next-piece]
                           (let [new-pos (new-tail-pos prior-piece next-piece)]
                             {:new-body (conj new-body new-pos)
                              :prior-piece new-pos}))
                         {:new-body [head-new]
                          :prior-piece head-new}
                         tail)]
    {:pieces new-body
     :visited (conj visited (last new-body))}))

  (comment
    (move-rope 
     {:pieces [[0 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0]]
      :visited #{[0 0]}}
     "R")
    
    (reduce move-rope
            {:pieces [[0 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0]]
             :visited #{[0 0]}}
            ["R" "R" "R" "R" "R" "R" "R" "R" "R" "R" "R" "R" "R" "R" "R" "R" "R" "R" "R"])
    ;
    )

; part 2
(->> "resources/day09.txt"
     read-steps
     (reduce move-rope
             {:pieces [[0 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0]]
              :visited #{[0 0]}})
     :visited
     count)