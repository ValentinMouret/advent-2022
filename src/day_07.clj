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
  "Parses the output of `ls` into a vector where the first element
  is the file name (or directory) and the optional second element is
  the size (if it’s a file)."
  [l]
  (let [matched (rest (re-find #"(dir|\d+) (.+)" l))
        f       (second matched)]
    (if (-> matched first (= "dir"))
      [f]
      [f (-> matched first parse-long)])))

(comment

  (parse-ls-output "4060174 j")
  (parse-ls-output "dir e")

  nil)

(defn parse-command
  [c]
  (let [[command & results] (str/split-lines c)
        [prog & args]       (str/split command #" ")]
    (case prog
      "cd" {:prog prog :args (first args)}
      "ls" {:prog prog :res  (map parse-ls-output results)})))

(defn parse-input
  [i]
  (let [commands (rest (str/split i #"\$ "))]
    (map parse-command commands)))

(defn step
  "Processes a step. If it’s a `cd`, changes the directory inside
  the state.
  If it’s an `ls`, update the tree in the state to reflect the output
  of `ls`, using the state’s pwd to know which keys to update.

  State
  -----
  :pwd  Value of the working directory as a stack. Contains the root `/`.
  :tree Filesystem we generated as a map.

  Example
  -------
  {:pwd (\"a\" \"/\")
   :tree {\"/\" {\"a\" {\"foo.txt\" 132832}
                 \"bar.csv\" 232323}}}
  "
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
  "Traverses the tree and squashes the directories into the following form:

  :size    Total size of the files *contained* in the directory.
  :folders Map of the folders containted in the directory.

  Note: The size is not «recursive». It does not reflect the size of the folders
        contained in the directory."
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
  "Goes through the tree to compute its total size, including sub-folders."
  [t]
  (+ (:size t)
     (->> t
          :folders
          vals
          (map total-size)
          (reduce +))))

(defn flatten-tree
  "Goes through the tree to create the sequence of
  directory and their total sizes. Each time the total size is recomputed, which
  is suboptimal.
  Ideally, there should be a bottom-up approach."
  [t]
  (->> t
       (map (fn unfold [[k v]]
              (conj (or (map unfold (:folders v)) list) {:name k :size (total-size v)})))
       (flatten)))

(defn input->flat-tree
  [i]
  (-> i
      parse-input
      generate-tree
      squash-sizes
      flatten-tree))

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
