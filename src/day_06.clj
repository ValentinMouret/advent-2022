(ns day-06
  (:require [clojure.java.io :as io]))

(def test-input
  "mjqjpqmgbljsphdztnvjfqwrcgsmlb")

(defn solve
  [i n]
  (let [tuples (partition n 1 i)]
    (reduce (fn [i xs]
              (if (-> xs set count (= n))
                (reduced (+ i n))
                (inc i)))
            0
            tuples)))

(solve test-input 4)
;; => 7

(-> "day_06.txt" (io/resource) (slurp) (solve 4))
;; => 1912

(solve "mjqjpqmgbljsphdztnvjfqwrcgsmlb" 14)
;; => 19

(-> "day_06.txt" (io/resource) (slurp) (solve 14))
;; => 2122
