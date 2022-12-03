(ns day-03
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.java.io :as io]))

(def test-input
  "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")

(defn parse-line
  [l]
  (let [half (/ (count l) 2)]
    (split-at half l)))

(defn process-line
  [l]
  (let [[l r] (parse-line l)]
    (first (set/intersection (set l) (set r)))))

(def alphabet-position
  (->> '(\a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z)
       (map-indexed (fn [i c] [c (inc i)]))
       (into {})))

(defn character-score
  [^Character c]
  (let [s (if (Character/isLowerCase c) 0 26)]
    (+ s (alphabet-position (Character/toLowerCase c)))))

(defn solve1
  [i]
  (let [lines (str/split-lines i)]
    (->> lines
         (map (comp character-score process-line))
         (reduce +))))

(solve1 test-input)
;; => 157

(let [i (-> "day_03.txt" (io/resource) (slurp))]
  (solve1 i))
;; => 8185

(defn solve2
  [i]
  (let [lines (str/split-lines i)]
    (->> lines
         (partition 3)
         (map (fn [t] (reduce set/intersection (map set t))))
         (map (comp character-score first))
         (reduce +))))

(solve2 test-input)
;; => 70

(let [i (-> "day_03.txt" (io/resource) (slurp))]
  (solve2 i))
;; => 2817
