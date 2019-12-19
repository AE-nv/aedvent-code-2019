(ns aoc-2019.day10
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.algo.generic.math-functions :as math]))

(defn parse-line
  ([line-nb line]
   (parse-line 0 line-nb line #{}))
  ([x y remaining acc]
   (if (seq remaining)
     (let [new-acc (if (= (first remaining) \#) (conj acc [x y]) acc)]
       (recur (inc x) y (rest remaining) new-acc))
     acc)))

(defn parse
  ([lines]
   (parse 0 lines #{}))
  ([line-nb remaining acc]
   (if (seq remaining)
     (recur
       (inc line-nb)
       (rest remaining)
       (set/union acc (parse-line line-nb (first remaining))))
     acc)))

(def all-asteroids (->> "input-10.txt" io/resource io/reader line-seq
                        parse))

(defn angle-with [[xc yc :as choosen-center]]
  (fn [[x y :as point]] (if (= choosen-center point)
                          "center"
                          (math/atan2 (- y yc) (- x xc)))))

(defn number-of-asteroids-visible-from [asteroid]
  (->>
    (group-by (angle-with asteroid) all-asteroids)
    keys
    (filter #(not= "center" %))
    count))

(defn part-1 []
  (->> all-asteroids
       (map number-of-asteroids-visible-from)
       (apply max)))

(defn part-2 []
  "todo")

(defn -main
  [& args]
  (println (str "Part 1: " (part-1)))
  (println (str "Part 2: " (part-2))))
