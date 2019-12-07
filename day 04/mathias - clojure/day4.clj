(ns aoc-2019.day4)

(defn str->numseq [s] (map #(read-string (str %)) s))

(defn double-present?
  ([previous remaining]
   (if (seq remaining)
     (let [current (first remaining)]
       (if (= previous current)
         true
         (double-present? current (rest remaining))))
     false)))

(defn ok? [x]
  (let [numseq (str->numseq (str x))]
    (and (double-present? (first numseq) (rest numseq))
         (= numseq (sort numseq)))))

(defn part-1 []
  (->>
    (range 124075 580769)
    (filter ok?)
    count))

(defn part-2 []
  "todo")

(defn -main
  [& args]
  (println (str "Part 1: " (part-1)))
  (println (str "Part 2: " (part-2))))