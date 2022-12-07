(ns day-07
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [clojure.java.io :as io]))

(def test-input
  "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")

(defn parse-ls-output
  [l]
  (let [dir      (rest (re-find #"dir (\w+)" l))
        [size f] (rest (re-find #"(\d+) (.+)" l))]
    (if size
      [f (parse-long size)]
      [(first dir)])))

(defn parse-command
  [c]
  (let [[command & results] (str/split-lines c)
        [prog & args] (str/split command #" ")]
    (case prog
      "cd" {:prog prog :args (first args)}
      "ls" {:prog prog :res (map parse-ls-output results)})))

(defn parse-input
  [i]
  (let [commands (rest (str/split i #"\$ "))]
    (map parse-command commands)))

(defn step
  [state {:keys [prog args res]}]
  (case prog
    "cd" (assoc state :pwd
                (cond
                  (= args "/")  (list "/")
                  (= args "..") (pop (:pwd state))
                  :else         (conj (:pwd state) args)))
    "ls" (let [p (-> state :pwd reverse (conj :tree))]
           (reduce (fn [s [f size]]
                     (update-in s p
                                assoc (str f) (or size {})))
                   state
                   res))))

(defn squash-sizes
  "Traverses the tree and squashes the directories to their sizes."
  [t]
  (:folders (walk/postwalk
             (fn [node]
               (if (map? node)
                 (let [s (->> node vals (filter number?) (reduce +))
                       r (->> node (filter (comp not number? val)) (into {}))]
                   (cond-> {:size s}
                     (seq r) (assoc :folders r)))
                 node))
             t)))

(defn generate-tree
  [steps]
  (let [state (reduce step {} steps)]
    (:tree state)))

(defn total-size
  [t]
  (+ (:size t)
     (->> t
          :folders
          vals
          (map (comp total-size))
          (reduce +))))

(defn flatten-tree
  [t]
  (->> t
       (reduce (fn aggregate [acc [k {:keys [folders] :as node}]]
                 (let [s (total-size node)]
                   (cond-> acc
                     :always (conj  {:name k :size s})
                     (seq folders) (concat acc (map #(aggregate (list) %) folders)))))
               (list))
       (flatten)))

(defn input->flat-tree
  [i]
  (-> i
      parse-input
      (generate-tree)
      (squash-sizes)
      (flatten-tree)))

(def maximum-folder-size 100000)

(defn solve1
  [i]
  (-> i
      input->flat-tree
      (->> (filter #(-> % :size (< maximum-folder-size)))
           (map :size)
           (reduce +))))

(def total-disk-space 70000000)
(def desired-free-space 30000000)

(defn solve2
  [i]
  (let [flat-tree  (input->flat-tree i)
        used-space (->> flat-tree (filter #(-> % :name (= "/"))) (first) :size)
        free-space (- total-disk-space used-space)
        to-free    (- desired-free-space free-space)]
    (->> flat-tree
         (filter #(-> % :size (> to-free)))
         (sort-by :size)
         (first)
         :size)))

(solve1 test-input)
;; => 95437

(-> "day_07.txt" (io/resource) (slurp) solve1)
;; => 1348005

(solve2 test-input)
;; => 24933642

(-> "day_07.txt" (io/resource) (slurp) solve2)
;; => 12785886
