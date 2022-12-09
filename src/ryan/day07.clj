(ns ryan.day07
  (:require [clojure.string :as str]
            [clojure.walk :as w]))

(def atom-for-partitioning (atom 0))

(defn split-on-commands [line]
  (if (= \$ (nth line 0))
    (swap! atom-for-partitioning inc)
    @atom-for-partitioning))

(defn read-blocks [file]
  (->> file
       slurp
       str/split-lines
       (drop 1)
       (partition-by split-on-commands)))

(defn cmd-ls [{:keys [path directory] :as m} cmd]
  (assoc m :directory (reduce (fn [s c]
                                (let [[x y] (str/split c #" ")]
                                  (if (= x "dir")
                                    (assoc-in s (conj path y) {})
                                    (assoc-in s (conj path y) (Integer/parseInt x)))))
                              directory
                              (drop 1 cmd))))

(defn cmd-cd [{:keys [path] :as m} [cmd]]
  (let [dir (subs cmd 5)]
    (assoc m :path (if (= dir "..")
                     (pop path)
                     (conj path dir)))))

(defn process-commands [cmds]
  (reduce (fn [m [cmd :as all]]
            (let [c (subs cmd 2 4)]
              (condp = c
                "cd" (cmd-cd m all)
                "ls" (cmd-ls m all))))
          {:directory {}
           :path []}
          cmds))

(comment
  (def sample
    {"a" {"e" {"i" 584}
          "f" 29116
          "g" 2557
          "h.lst" 62596}
     "b.txt" 14848514
     "c.dat" 8504156
     "d" {"j" 4060174
          "d.log" 8033020
          "d.ext" 5626152
          "k" 7214296}})

  (cmd-ls {:directory {}
           :path []}
          '("$ ls" "dir a" "14848514 b.txt" "8504156 c.dat" "dir d"))
  (cmd-ls {:direcotry {"hello" {"world" {}}}
           :path ["hello"]}
          '("$ ls" "dir a" "14848514 b.txt" "8504156 c.dat" "dir d"))
  (cmd-cd {:directory {:hello :world}
           :path ["a"]}
          '("$ cd efg"))
  (cmd-cd {:directory {:hello :world}
           :path ["a" "b" "c"]}
          '("$ cd .."))
  ;
  )

(defn find-small-directories [directory]
  (let [smaller-than-100k (atom [])]
    (w/postwalk (fn [x]
                  (if (map? x)
                    (let [rollup (->> x
                                      (map second)
                                      (reduce +))]
                      (when (< rollup 100000)
                        (swap! smaller-than-100k conj rollup))
                      rollup)
                    x))
                directory)
    @smaller-than-100k))

; part 1
(->> "resources/day07.txt"
     read-blocks
     process-commands
     :directory
     find-small-directories
     (reduce +))



; part 2
(->> "resources/day07.txt"
     read-blocks
     process-commands
     :directory
     (w/postwalk (fn [x]
                   (if (map? x)
                     (->> x
                          (map second)
                          (reduce +))
                     x))))
; => 44274331

(- 44274331 (- 70000000 30000000)) ; 4274331

(defn find-smallest-above-4274331 [directory]
  (let [smallest-above-4274331 (atom Integer/MAX_VALUE)]
    (w/postwalk (fn [x]
                  (if (map? x)
                    (let [rollup (->> x
                                      (map second)
                                      (reduce +))]
                      (when (and (> rollup 4274331)
                                 (< rollup @smallest-above-4274331))
                        (reset! smallest-above-4274331 rollup))
                      rollup)
                    x))
                directory)
    @smallest-above-4274331))

(->> "resources/day07.txt"
     read-blocks
     process-commands
     :directory
     find-smallest-above-4274331)
