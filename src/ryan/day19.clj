(ns ryan.day19
  (:require [clojure.string :as str]))

;; from *now*, branch 4 times, once per building the next machine of each type
;; - fast forward through building the machine for that branch
;;   - if you fully lack income of a machine type, then the branch will die
;; - rather inefficient branches should die out
;; - if you already have enough machines >= highest cost across all machines for a type
;;   then kill the branch
;; - bfs/dfs these

(defn read-robot [robot]
  (let [[_ robot-type  _ _ amt-1 res-1 _ amt-2 res-2] (str/split robot #" ")]
    {(keyword robot-type) (cond-> {(keyword res-1) (parse-long amt-1)}
                            amt-2 (assoc (keyword res-2) (parse-long amt-2)))}))

(defn read-blueprint [line]
  (->> (str/split line #"[.:]\s?")
       (drop 1)
       (map read-robot)
       (reduce merge)))

(defn read-blueprints [file]
  (->> file
       slurp
       str/split-lines
       (map read-blueprint)))

(comment
  (read-robot "Each ore robot costs 3 ore")
  (read-blueprint "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.")
  (read-blueprints "resources/day19-sample.txt")

  ;
  )

(def types [:ore :clay :obsidian :geode])
(defn produce [resources robots]
  (reduce (fn [acc k]
            (update acc k + (robots k)))
          resources
          types))

(defn spend [resources cost]
  (reduce (fn [acc k]
            (update acc k - (get cost k 0)))
          resources
          types))

(defn has-enough-resources [{:keys [ore clay obsidian]}
                            {ore-cost :ore clay-cost :clay obsidian-cost :obsidian}]
  (and (or (nil? ore-cost) (>= ore ore-cost))
       (or (nil? clay-cost) (>= clay clay-cost))
       (or (nil? obsidian-cost) (>= obsidian obsidian-cost))))

(def get-most-needed
  (memoize (fn [blueprint robot]
             (println "math!")
             (apply max (map #(get (second %) robot 0) blueprint))
                        ;;  7
                        ;; (condp = robot
                        ;;   :ore 3
                        ;;   :clay 6
                        ;;   :obsidian 10
                        ;;   :geode 99)

                        ;; determined heuristically (basically 75% of the max value)
            ;;  (apply max (map #(quot (* 3 (get (second %) robot 0)) 4) blueprint))
             )))

;; nil means can't be built (we go over max steps),
;; or shouldn't be built (building more of this type isn't helpful)
(defn fast-forward-till-next-built [{:keys [resources robots step]} blueprint robot max-steps]
;;   (println "ff" resources robots step blueprint robot max-steps)
  (let [cost (robot blueprint)
        most-needed (get-most-needed blueprint robot)
        ]
    (loop [res resources
           s step]
    ;;   (println "loop" res s)
      (cond
        (and (not= :geode robot) (>= (robots robot) most-needed)) nil
        (>= s max-steps) nil
        (has-enough-resources res cost) {:resources (-> res
                                                        (produce robots)
                                                        (spend cost))
                                         :robots (update robots robot inc)
                                         :step (inc s)}
        :else (recur (produce res robots)
                     (inc s))))))

(defn fast-forward-till-end [{:keys [resources robots step]} max-steps]
  (loop [res resources
         s step]
    (if (>= s max-steps)
      res
      (recur (produce res robots)
             (inc s)))))

(comment
  (let [resources {:ore 1 :clay 2 :obsidian 3 :geode 4}
        robots {:ore 5 :clay 6 :obsidian 7 :geode 8}]
    (produce resources robots))

  (let [resources {:ore 2 :clay 20 :obsidian 20 :geode 0}
        blueprint (read-blueprint "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.")]
    (has-enough-resources resources (:geode blueprint)))

  (let [resources {:ore 4 :clay 20 :obsidian 20 :geode 0}
        blueprint (read-blueprint "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.")]
    (spend resources (:ore blueprint)))

  (let [resources {:ore 0 :clay 0 :obsidian 0 :geode 0}
        robots {:ore 1 :clay 0 :obsidian 0 :geode 0}
        blueprint (read-blueprint "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.")]
    (fast-forward-till-next-built {:resources resources
                                   :robots robots
                                   :step 0} blueprint :clay 24))

  (let [resources {:ore 1 :clay 0 :obsidian 0 :geode 0}
        robots {:ore 1 :clay 0 :obsidian 0 :geode 1}]
    (fast-forward-till-end {:resources resources
                            :robots robots
                            :step 0} 24))
  ;
  )

(defn find-best-route [blueprint max-steps]
  (loop [[path & others] '({:resources {:ore 0 :clay 0 :obsidian 0 :geode 0}
                            :robots {:ore 1 :clay 0 :obsidian 0 :geode 0}
                            :step 0})
         most-geodes 0
         i 0]
    ;; (Thread/sleep 1000)
    ;; (println "loop" (:step path) (count others) most-geodes)
    ;; (println (:robots path))
    (when (zero? (mod i 1000000))
      (println "loop" (:step path) (count others) ((juxt :ore :clay :obsidian :geode) (:robots path))))
    (if (nil? path)
      most-geodes
      (let [next-ore (fast-forward-till-next-built path blueprint :ore max-steps)
            next-clay (fast-forward-till-next-built path blueprint :clay max-steps)
            next-obsidian (fast-forward-till-next-built path blueprint :obsidian max-steps)
            next-geode (fast-forward-till-next-built path blueprint :geode max-steps)]
        ;; (println "nexts" next-ore next-clay next-obsidian next-geode)
        ;; (when next-geode
        ;;   (println next-geode))
        ;; (println "nexts" next-ore next-clay next-obsidian next-geode)
        (if (or next-ore next-clay next-obsidian next-geode)
          (recur (cond-> others
                   next-ore (conj next-ore)
                   next-clay (conj next-clay)
                   next-obsidian (conj next-obsidian)
                   next-geode (conj next-geode))
                 most-geodes
                 (inc i))
          (recur others
                 (let [this-max (:geode (fast-forward-till-end path max-steps))]
                   (when (> this-max most-geodes)
                     (println "found better!" (:robots path) (:step path) this-max))
                   (max most-geodes this-max))
                 (inc i)))))))


(find-best-route (read-blueprint "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.")
                 32)

(find-best-route (read-blueprint "Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.")
                 32)

;; part 1
(->> "resources/day19-sample.txt"
     read-blueprints
     (map #(find-best-route % 24))
     (map-indexed (fn [i r] (* (inc i) r)))
     (reduce +))

;; part 2
;; (->> "resources/day19.txt"
;;      read-blueprints
;;      (take 3)
;;      (map #(find-best-route % 32))
;;      (reduce *))

(find-best-route (read-blueprint "Blueprint 1: Each ore robot costs 3 ore. Each clay robot costs 3 ore. Each obsidian robot costs 2 ore and 19 clay. Each geode robot costs 2 ore and 12 obsidian.")
                 32)
;; 22

(find-best-route (read-blueprint "Blueprint 2: Each ore robot costs 3 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 19 clay. Each geode robot costs 2 ore and 9 obsidian.")
                 32)
;; 29

(find-best-route (read-blueprint "Blueprint 3: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 17 clay. Each geode robot costs 3 ore and 10 obsidian.")
                 32)

(* 22 29 27)