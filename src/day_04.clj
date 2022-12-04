(ns day-04
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def test-input
  "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

(defn parse-line
  [l]
  (->> l
       (re-find #"(\d+)-(\d+),(\d+)-(\d+)")
       (rest)
       (map parse-long)))

(defn parse-input
  [i]
  (->> i
       (str/split-lines)
       (map parse-line)))

(defn solve
  [i pred]
  (->> i
       (parse-input)
       (filter pred)
       (count)))

(defn inclusives?
  [[left-lower left-upper right-lower right-upper]]
  (or (and (<= right-upper left-upper) (>= right-lower left-lower))
      (and (<= left-upper right-upper) (>= left-lower right-lower))))

(solve test-input inclusives?)
;; => 2
(-> "day_04.txt" (io/resource) (slurp) (solve inclusives?))
;; => 496

(defn exclusives?
  [[left-lower left-upper right-lower right-upper]]
  (or (< left-upper right-lower)
      (< right-upper left-lower)))

(def overlaps? (complement exclusives?))

(solve test-input overlaps?)
;; => 4

(-> "day_04.txt" (io/resource) (slurp) (solve overlaps?))
;; => 847
