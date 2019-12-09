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

(defn ascending-numbers? [numseq] (= numseq (sort numseq)))
(defn number->numseq [x] (str->numseq (str x)))

(defn ok? [x]
      (let [numseq (number->numseq x)]
           (and (double-present? (first numseq) (rest numseq))
                (ascending-numbers? numseq))))

(defn part-1 []
      (->>
        (range 124075 580769)
        (filter ok?)
        count))

(defn series-size-by-number
      [numseq]
      ((reduce
         (fn [{previous :prev frequencies :freq :as acc} current]
             (if (= current previous)
               {:prev current
                :freq (update frequencies current #(if % (inc %) 2))}
               {:prev current :freq frequencies}))
         {}
         numseq) :freq))

(defn double-present-but-no-more?
      [numseq]
      (->> numseq
           series-size-by-number
           vals
           (some #(= 2 %))))

(defn part-2 []
      (->>
        (range 124075 580769)
        (map number->numseq)
        (filter ascending-numbers?)
        (filter double-present-but-no-more?)
        count))

(defn -main
      [& args]
      (println (str "Part 1: " (part-1)))
      (println (str "Part 2: " (part-2))))