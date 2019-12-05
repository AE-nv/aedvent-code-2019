(ns aoc-2019.day2
    (:require [clojure.java.io :as io]
      [clojure.string :as str]))

(def line (->> "input-2.txt" io/resource io/reader slurp))
(def code
  (->>
    (str/split line #",")
    (map read-string)
    (into [])))

(defn load-noun-verb [noun verb]
      (assoc (assoc code 1 noun) 2 verb))

(def fixed-code (load-noun-verb 12 2))

(def operation-map {1 + 2 *})

(defn execute
      ([code]
       (execute 0 code))
      ([index code]
       (let [op-code (get code index)]
            (if (= 99 op-code)
              (first code)
              (let [operation (operation-map op-code)
                    arg1-position (code (+ index 1))
                    arg2-position (code (+ index 2))
                    output-position (code (+ index 3))
                    result (operation (code arg1-position) (code arg2-position))]
                   (execute (+ index 4) (assoc code output-position result)))))))

(defn part-1 []
      (execute fixed-code))

(def lazy-noun-verb-pairs
  (for [noun (range 1 100)
        verb (range 1 100)]
       [noun verb]))

(def target 19690720)

(def find-first-transducer
  (comp
    (map (fn [[noun verb]] {:pair [noun verb] :result (execute (load-noun-verb noun verb))}))
    (take-while (fn [{calculation :result}] (not (> calculation target))))
    (map :pair)))

(defn part-2 []
      (let [[noun verb] (first (transduce find-first-transducer conj '() lazy-noun-verb-pairs))]
           (-> noun (* 100) (+ verb))))

(defn -main
      [& args]
      (println (str "Part 1: " (part-1)))
      (println (str "Part 2: " (part-2))))