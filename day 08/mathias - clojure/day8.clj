(ns aoc-2019.day8
    (:require [clojure.java.io :as io]))

(def layers (->> "input-8.txt" io/resource io/reader slurp
                 (partition (* 25 6))))

(defn part-1 []
      (->> layers
           (map frequencies)
           (apply min-key (fn [map] (map \0)))
           ((fn [map] (* (map \1) (map \2))))))

(def through-section
  (->> layers
       (apply interleave)
       (partition (count layers))))

(defn part-2 []
      (let [final-image (->> through-section
                             (map (partial some #{\0 \1}))
                             (apply str))]
           (map println (partition 25 final-image))))

(defn -main
      [& args]
      (println (str "Part 1: " (part-1)))
      (println (str "Part 2: " (part-2))))
