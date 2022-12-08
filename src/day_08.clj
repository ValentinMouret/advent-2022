(ns day-08
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def test-input
  "30373
25512
65332
33549
35390")

(defn parse-input
  [i]
  (->> i
       (str/split-lines)
       (mapv (fn [r] (mapv #(Character/digit % 10) r)))))

(defn transpose
  [matrix]
  (apply mapv vector matrix))

(defn solver
  [g]
  (let [transposed (transpose g)
        n          (count g)
        m          (count (first g))]
    (fn blocked?
      [i j]
      (let [row  (get g i)
            col  (get transposed j)
            x    (get row j)
            values (list (set (subvec row 0 j))
                         (set (subvec row (inc j) n))
                         (set (subvec col 0 i))
                         (set (subvec col (inc i) m)))]
        (->> values
             (map (fn [vs] (some (fn [v] (>= v x)) vs)))
             (filter some?)
             (count)
             (= 4))))))

(defn solve1
  [i]
  (let [g (parse-input i)
        visible? (complement (solver g))]
    (->> (for [i (range (count g))
               j (range (count (first g)))]
           (when (visible? i j)
             [i j (get (get g i) j) (visible? i j)]))
         (filter some?)
         (count))))

(solve1 test-input)
;; => 25

(-> "day_08.txt" (io/resource) (slurp) (solve1))
;; => 1859

(defn view-range
  [h coll]
  (reduce
   (fn [acc v]
     (let [cont (conj acc v)]
       (if (>= v h) (reduced cont) cont)))
   '()
   coll))

(defn scenic-score-solver
  [g]
  (let [transposed (transpose g)
        n          (count g)
        m          (count (first g))]
    (fn scenic-score
      [i j]
      (let [row  (get g i)
            col  (get transposed j)
            x    (get row j)
            values (list (reverse (subvec row 0 j))
                         (subvec row (inc j) n)
                         (reverse (subvec col 0 i))
                         (subvec col (inc i) m))]
        (->> values
             (map (comp count (partial view-range x)))
             (reduce *))))))

(defn solve2
  [i]
  (let [g (parse-input i)
        scorer (scenic-score-solver g)]
    (apply max (for [i (range (count g))
                     j (range (count (first g)))]
                 (scorer i j)))))

(solve2 test-input)
;; => 8

(-> "day_08.txt" (io/resource) (slurp) (solve2))
;; => 332640
