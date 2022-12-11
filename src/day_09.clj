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

(defn parse-input
  [i]
  (->> i
       (str/split-lines)
       (map (fn [s] (let [[_ [d] m] (re-find #"(\w) (\d+)" s)]
                      [d (parse-long m)])))))

(defn adjacent?
  [[x y] [x' y']]
  (and (<= (abs (- x x')) 1)
       (<= (abs (- y y')) 1)))

(defn move
  [[x y] m d]
  (case m
    \U [x (+ y d)]
    \D [x (- y d)]
    \R [(+ x d) y]
    \L [(- x d) y]))

(defn manhattan-distance
  [[x y] [x' y']]
  (+ (abs (- x x'))
     (abs (- y y'))))

(defn catchup
  [[x y] [x' y']]
  (if (adjacent? [x y] [x' y'])
    [x' y']
    (let [dx (- x x')
          dy (- y y')]
      (case (manhattan-distance [x y] [x' y'])
        0 [x' y']
        1 [x' y']
        2 (let [direction (cond
                            (= dx -2) \L
                            (= dx 2)  \R
                            (= dy -2) \D
                            (= dy 2)  \U)]
            (move [x' y'] direction 1))
        3 (-> [x' y']
              (move (if (> dx 0) \R \L) 1)
              (move (if (> dy 0) \U \D) 1))))))

(defn unroll
  [steps]
  (reduce
   (fn [[[h t] & b4] [d m]] (reduce (fn [[[h' t'] & b4'] _]
                                      (let [h'' (move h' d 1)
                                            t'' (catchup h'' t')]
                                        (conj (conj b4' [h' t']) [h'' t''])))
                                    (conj b4 [h t])
                                    (range m)))
   (list [[0 0] [0 0]])
   steps))

(defn solve1
  [i]
  (let [steps (parse-input i)
        states (unroll steps)]
    (->> states
         (map second)
         (into #{})
         (count))))

(->> (unroll (parse-input test-input))
     (map second)
     (into #{})
     (count))

(solve1 test-input)
;; => 13

(-> "day_09.txt" (io/resource) (slurp) (solve1))
;; => 6212

;; Turn the cells into a stack of max size 10.
;; (if (and (< 10 (count cells))
;;          (not= [0 0] (last cells))
;;     (generate new one here))
;;
