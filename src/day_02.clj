(ns day-02
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(def opponent-encoding
  {\A :rock
   \B :paper
   \C :scissors})

(def player-encoding
  "This is the first encoding we assume for the second column."
  {\X :rock
   \Y :paper
   \Z :scissors})

(def hand-score
  {:rock     1
   :paper    2
   :scissors 3})

(def outcome-score
  {:loss 0
   :draw 3
   :win  6})

(def wins-against
  {:rock     :scissors
   :paper    :rock
   :scissors :paper})
(def possible-hands (set (vals wins-against)))

(defn outcome
  [opponent-hand player-hand]
  (let [losing-hand (wins-against opponent-hand)]
    (cond
      (= player-hand opponent-hand) :draw
      (= player-hand losing-hand)   :loss
      :else                         :win)))

(defn parse-game
  [[o _ p]]
  (let [opponent-hand (opponent-encoding o)
        player-hand   (player-encoding p)]
    {:hands   {:opponent opponent-hand
               :player   player-hand}
     :outcome (outcome opponent-hand player-hand)}))

(defn score
  [g]
  (+ (-> g :hands :player hand-score)
     (-> g :outcome outcome-score)))

(def test-input
  "A Y
B X
C Z")

(defn solve1
  "Given a puzzle input, finds the total score of the user
  in the games."
  [i]
  (let [games (str/split-lines i)]
    (->> games
         (map (comp score parse-game))
         (reduce +))))

(solve1 test-input)
;; => 15

(let [i (-> "day_02.txt" io/resource slurp)]
  (solve1 i))
;; => 13009

(def game-outcome-encoding
  {\X :loss
   \Y :draw
   \Z :win})

(defn reverse-hand
  [outcome opponent-hand]
  (let [draw-hand      opponent-hand
        losing-hand    (wins-against opponent-hand)
        winning-hand   (first (set/difference possible-hands #{draw-hand losing-hand}))]
    (case outcome
      :draw draw-hand
      :loss losing-hand
      :win  winning-hand)))

(defn parse-game2
  [[o _ p]]
  (let [opponent-hand (opponent-encoding o)
        outcome       (game-outcome-encoding p)]
    {:hands    {:opponent opponent-hand
                :player   (reverse-hand outcome opponent-hand)}
     :outcome outcome}))

(defn solve2
  [i]
  (let [games (str/split-lines i)]
    (->> games
         (map (comp score parse-game2))
         (reduce +))))

(solve2 test-input)
;; => 12
(let [i (-> "day_02.txt" io/resource slurp)]
  (solve2 i))
;; => 10398

;; Improvements:
;;
;; `solve1` and `solve2` are very similar here. In fact, only the parsing function
;; changes. A more elegant method would be to turns `solve` into a higher order function.
;;
;; Also, the code here is by no means geared towards optimisations.
;; The parsing of the game might induce overhead, but it helps understanding
;; what’s happening and debugging.

(defn solve'
  [i parser]
  (let [games  (str/split-lines i)]
    (->> games
         (map (comp score parser))
         (reduce +))))

(solve' test-input parse-game)
;; => 15
(solve' test-input parse-game2)
;; => 12

(let [i (-> "day_02.txt" io/resource slurp)]
  [(solve' i parse-game)
   (solve' i parse-game2)])
;; => [13009 10398]
