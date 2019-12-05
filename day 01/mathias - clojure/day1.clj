(ns aoc-2019.day1
  (:require [clojure.java.io :as io]))

(def masses (->> "input-1.txt"
                 io/resource
                 io/reader
                 line-seq
                 (map read-string)))

(defn mass->fuel [mass] (-> mass (/ 3) (+ -2) int))

(defn part-1 []
  (->> masses
       (map mass->fuel)
       (reduce +)))

(defn mass->total-fuel
  [mass]
  (let [fuel-requirement (mass->fuel mass)]
    (if (> fuel-requirement 0)
      (+ fuel-requirement (mass->total-fuel fuel-requirement))
      0)))

(defn part-2 []
  (->> masses
       (map mass->total-fuel)
       (reduce +)))

(defn -main
  [& args]
  (println (str "Part 1: " (part-1)))
  (println (str "Part 2: " (part-2))))