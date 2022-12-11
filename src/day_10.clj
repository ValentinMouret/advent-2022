(ns day-10
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def test-input
  "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop")

(defn parse-line
  [l]
  (let [[_ op arg] (re-find #"(noop|addx )(.*)" l)]
    (case op
      "noop" {:op :noop}
      "addx " {:op :addx :dv (parse-long arg)})))

(defn parse-input
  [i]
  (->> i
       (str/split-lines)
       (map parse-line)))

(def initial-value
  {:cycles 0
   :during 1
   :after  1})

(defn draw-char
  [{:keys [during cycles] :as s}]
  (let [position (mod (dec cycles) 40)]
    (assoc s
           :char
           (if (<= (dec during) position (inc during)) \# \.))))

(defn process
  [state {:keys [op dv]}]
  (let [last-state (peek state)
        pre        (-> last-state
                       (update-in [:cycles] inc)
                       (assoc :during (:after last-state))
                       (draw-char))]
    (cond-> state
      (= op :noop) (conj pre)
      (= op :addx) (conj pre
                         (-> pre
                             (update-in [:cycles] inc)
                             (update-in [:after]  #(+ % dv))
                             (draw-char))))))

(defn solve1
  [i]
  (let [parsed (parse-input i)]
    (->> parsed
         (reduce process (list initial-value))
         (filter (fn [s] (-> s :cycles (+ 20) (mod 40) (= 0))))
         (map (fn [{:keys [cycles during]}] (* cycles during)))
         (reduce +))))

(solve1 test-input)
;; => 13140

(-> "day_10.txt" (io/resource) (slurp) (solve1))
;; => 12560

(defn solve2
  [i]
  (->>
   i
   (parse-input)
   (reduce process (list initial-value))
   (reverse)
   (rest)
   (map :char)
   (partition 40)
   (map (partial str/join ""))))

(-> "day_10.txt" (io/resource) (slurp) (solve2))
;; => ("###..#....###...##..####.###...##..#...."
;;     "#..#.#....#..#.#..#.#....#..#.#..#.#...."
;;     "#..#.#....#..#.#..#.###..###..#....#...."
;;     "###..#....###..####.#....#..#.#....#...."
;;     "#....#....#....#..#.#....#..#.#..#.#...."
;;     "#....####.#....#..#.#....###...##..####.")
;; PLPAFBCL
