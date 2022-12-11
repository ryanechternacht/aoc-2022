(ns ryan.day11
  (:require [clojure.string :as str]))

(defn read-monkeys [file]
  (->> file
       slurp
       str/split-lines
       (partition 7 7 [""])
       (map (fn [[_ items-line op-line test-line true-line false-line]]
              (let [items (->> items-line
                               (#(subs % 18))
                               (#(str/split % #", "))
                               (map parse-long)
                               vec)
                    [_ _ _ _ _ _ op-symbol op-n-str] (str/split op-line #" ")
                    op-n (parse-long op-n-str)
                    op (cond
                         (and op-n (= op-symbol "*")) #(* % op-n)
                         (and op-n (= op-symbol "+")) #(+ % op-n)
                         (= op-symbol "*") #(* % %))
                    test-n (parse-long (subs test-line 21))
                    true-n (parse-long (subs true-line 29))
                    false-n (parse-long (subs false-line 30))]
                {:items items
                 :operation op
                 :test-num test-n
                 :true-throw true-n
                 :false-throw false-n
                 :checks 0})))
       vec))


(def reduce-worry #(int (/ % 3)))

(defn act-on-item [{:keys [operation test-num true-throw false-throw]} crt-nums item]
  (let [after-worry (-> item
                        operation
                        ;; reduce-worry ; removed for part 2
                        (#(mod % (apply * crt-nums))))]
    {:worry after-worry
     :to-monkey (if (= 0 (mod after-worry test-num)) true-throw false-throw)}))

(defn monkey-acts [monkeys n]
  (let [{:keys [items] :as monkey} (monkeys n)
        crt-nums (map :test-num monkeys)]
    (reduce (fn [acc item]
              (let [{:keys [worry to-monkey]} (act-on-item monkey crt-nums item)]
                (update-in acc [to-monkey :items] conj worry)))
            ;; clear monkey's items and update check count
            (-> monkeys
                (assoc-in [n :items] [])
                (update-in [n :checks] + (count items)))
            items)))

(defn monkeys-round [monkeys]
  (reduce monkey-acts
          monkeys
          (range (count monkeys))))

(comment

  (def sample-monkeys (read-monkeys "resources/day11-sample.txt"))
  (act-on-item (sample-monkeys 0) [23 19 13 17] 79)
  (monkey-acts sample-monkeys 0)
  (monkeys-round sample-monkeys)
  ; 
  )

; part 1
(->> (reduce (fn [acc _] (monkeys-round acc)) 
             (read-monkeys "resources/day11.txt") 
             (range 10000))
     (map :checks)
     sort
     reverse
     (take 2)
     (reduce *))
