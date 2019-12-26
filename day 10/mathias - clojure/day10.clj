(ns aoc-2019.day10
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.algo.generic.math-functions :as math]
            [clojure.math.numeric-tower :as num]))

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

(defn distance-from [[xc yc :as choosen-center]]
  (fn [[x y :as point]]
    (->> (list [xc x] [yc y])
         (map (partial apply -))
         (map #(num/expt % 2))
         (apply +)
         num/sqrt)))

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

(defn by-count [asteroid]
  [(number-of-asteroids-visible-from asteroid) asteroid])

(def the-spot (->> all-asteroids
                   (map by-count)
                   (apply max-key first)
                   second))

(def all-other-asteroids (filter #(not= % the-spot) all-asteroids))
(def up ((angle-with [0 0]) [0 -1]))
(def down ((angle-with [0 0]) [0 1]))
(def right ((angle-with [0 0]) [1 0]))
(def left ((angle-with [0 0]) [-1 0]))

(def pi (math/atan2 0 -1))
(def two-pi (* 2 pi))

(defn rotate-up [angle] (+ angle (/ pi 2)))
(defn angle->positive-angle [angle] (if (neg? angle) (+ two-pi angle) angle))
(def angle-with-center (comp angle->positive-angle rotate-up (angle-with the-spot)))

(def asteroids-by-angle-and-distance
  (->> all-other-asteroids
       (group-by angle-with-center)
       (into (sorted-map))
       vals
       (map #(sort-by (distance-from the-spot) %))))

(defn interleave-all
  "interleaves including remainder of longer seqs."
  [& seqs]
  (if (not-empty (first seqs))
    (cons (first (first seqs))
          (lazy-seq (apply interleave-all (filter not-empty (concat (rest seqs) [(rest (first seqs))])))))))

(def destruction-sequence (apply interleave-all asteroids-by-angle-and-distance))

(defn part-2 []
  (last (take 200 destruction-sequence)))

(defn -main
  [& args]
  (println (str "Part 1: " (part-1)))
  (println (str "Part 2: " (part-2))))
