(ns day-05
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def test-input (-> "day_05_test.txt" (io/resource) (slurp)))

(defn parse-stacks
  [stacks]
  (->> stacks
       (str/split-lines)
       (reverse)
       (rest)
       (map (fn [r] (let [matches (re-seq #"\[(\w)\]|(\s{4})" r)]
                      (map second matches))))
       (reduce (fn [acc coll] (map #(if %2 (conj %1 %2) %1) acc coll))
               (repeat (list)))
       (into [])))

(defn parse-moves
  [i]
  (->> i
       (str/split-lines)
       (map (comp rest (partial re-find #"move (\d+) from (\d+) to (\d+)")))
       (map (partial map parse-long))))

(defn parse-input
  [i]
  (let [[stacks moves] (str/split i #"\R\R")]
    [(parse-stacks stacks)
     (parse-moves moves)]))

(parse-input test-input)

(defn step1
  [stacks [n from to]]
  (let [from-s (get stacks (dec from))
        to-s   (get stacks (dec to))]
    (-> stacks
        (assoc (dec from) (reduce (fn [coll _] (pop coll))
                                  from-s
                                  (range n)))
        (assoc (dec to)   (reduce conj
                                  to-s
                                  (take n from-s))))))

(defn solve
  [i stepper]
  (let [[stacks moves] (parse-input i)
        final          (reduce stepper stacks moves)]
    (->> final
         (map peek)
         str/join)))

(solve test-input step1)
;; => "CMZ"

(let [i (-> "day_05.txt" (io/resource) (slurp))]
  (solve i step1))
;; => "FCVRLMVQP"

(defn step2
  [stacks [n from to]]
  (let [from-s (get stacks (dec from))
        to-s   (get stacks (dec to))]
    (-> stacks
        (assoc (dec from)  (reduce (fn [coll _] (pop coll))
                                   from-s
                                   (range n)))
        (assoc (dec to)   (reduce conj
                                  to-s
                                  (reverse (take n from-s)))))))

(solve test-input step2)
;; => "MCD"

(let [i (-> "day_05.txt" (io/resource) (slurp))]
  (solve i step2))
;; => "RWLWGJGFD"
