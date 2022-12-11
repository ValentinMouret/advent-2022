(ns day-09
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def test-input
  "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

(def moves
  {\U [0 1]
   \D [0 -1]
   \R [1 0]
   \L [-1 0]})

(defn parse-input
  [i]
  (->> i
       (str/split-lines)
       (mapcat (fn [s]
                 (let [[_ [d] m] (re-find #"(\w) (\d+)" s)
                       m         (parse-long m)]
                   (repeat m (moves d)))))))

(defn catchup [h t]
  (let [dist (map - h t)]
    (if (some #(< 1 (abs %)) dist)
      (->> dist
           (map #(Long/signum %))
           (map + t))
      t)))

(defn step
  [[h & rest] d]
  (reductions catchup
              (map + h d)
              rest))

(defn unroll
  [steps n]
  (let [knots (repeat n [0 0])]
    (reductions step knots steps)))

(defn solve
  [i n]
  (let [steps  (parse-input i)
        states (unroll steps n)]
    (->> states
         (map last)
         (into #{})
         (count))))

(solve test-input 2)
;; => 13

(-> "day_09.txt" (io/resource) (slurp) (solve 2))
;; => 6212

(def test-input-2
  "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20")

(solve test-input-2 10)
;; => 36

(-> "day_09.txt" (io/resource) (slurp) (solve 10))
;; => 2522
