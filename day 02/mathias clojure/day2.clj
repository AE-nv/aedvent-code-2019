(ns aoc-2019.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def line (->> "input-2.txt" io/resource io/reader slurp))
(def code
  (->>
    (str/split line #",")
    (map read-string)
    (into [])))
(def fixed-code (assoc (assoc code 1 12) 2 2))

(def operation-map {1 + 2 *})

(defn execute
  ([code]
   (execute 0 code))
  ([index code]
   (let [op-code (get code index)]
     (if (= 99 op-code)
       code
       (let [operation (operation-map op-code)
             arg1-position (code (+ index 1))
             arg2-position (code (+ index 2))
             output-position (code (+ index 3))
             result (operation (code arg1-position) (code arg2-position))]
         (execute (+ index 4) (assoc code output-position result)))))))

(defn part-1 []
  (execute 0 fixed-code))

(defn part-2 []
  "todo")

(defn -main
  [& args]
  (println (str "Part 1: " (part-1)))
  (println (str "Part 2: " (part-2))))