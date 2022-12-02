(ns day-01
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

;; The Elves take turns writing down the number of Calories contained by the various meals,
;; snacks, rations, etc. that they've brought with them, one item per line.
;; Each Elf separates their own inventory from the previous Elf's inventory (if any) by a blank line.

(def test-input
  "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000")

(defn parse-input
  [i]
  (->> (str/split i #"\R\R")
       (map (fn [lines] (map parse-long (str/split-lines lines))))))

(defn solve1
  "Given an input puzzle in text format, we want to find how much calories the Elf
  which with the most calories has."
  [i]
  (let [parsed (parse-input i)
        totals (map (partial reduce +) parsed)]
    (->>  totals
          (sort)
          (last))))

(solve1 test-input)
;; => 24000

(let [input (-> "day_01.txt" io/resource slurp)]
  (solve1 input))
;; => 72070

(defn solve2
  "Given an input puzzle in text format, returns the top three
  Elves with the most snacks."
  [i]
  (let [parsed (parse-input i)
        totals (map (partial reduce +) parsed)]
    (->>  totals
          (sort)
          (reverse)
          (take 3)
          (reduce +))))

(solve2 test-input)
;; => 45000

(let [input (-> "day_01.txt" io/resource slurp)]
  (solve2 input))
;; => 211805
