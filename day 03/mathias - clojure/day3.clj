(ns aoc-2019.day3
    (:require [clojure.java.io :as io]
      [clojure.string :as str]
      [clojure.set :as set]))

(def lines (->> "input-3.txt" io/resource io/reader line-seq))

(defn parse-instruction [[x & rest]]
      {:direction (keyword (str x))
       :steps     (->> rest (apply str) (. Integer valueOf))})

(defn parse-path [path-str]
      (->>
        (str/split path-str #",")
        (map parse-instruction) ))

(def step-function-map
  {:R (fn [[x y]] [(inc x) y])
   :L (fn [[x y]] [(dec x) y])
   :U (fn [[x y]] [x (inc y)])
   :D (fn [[x y]] [x (dec y)])})

(defn apply-instruction [instruction start]
      (let [func (step-function-map (:direction instruction))]
           (loop [remaining-steps (:steps instruction)
                  result '()]

                 (if (= remaining-steps 0)
                   result
                   (let [previous (if (empty? result) start (first result))
                         next (func previous)]
                        (recur (dec remaining-steps) (cons next result)))))))

(def start-point [0 0])

(defn construct [instructions]
      (reduce (fn [acc inst] (concat (apply-instruction inst (first acc)) acc))
              (list start-point)
              instructions))

(defn get-path [path-str]
      (->> path-str
           parse-path
           construct
           reverse))

(defn get-point-set-for-path [path-str] (into #{} (get-path path-str)))

(def intersections
  (->> lines
       (map get-point-set-for-path)
       (apply set/intersection)
       (filter #(not= % start-point))
       (into #{})))

(defn abs [val] (max val (- val)))
(defn point->manhattan-distance [[x y :as point]] {:dist (+ (abs x) (abs y)) :point point})

(defn part-1 []
      (->> intersections
           (map point->manhattan-distance)
           (apply min-key :dist)))


(defn point->position [points-set path]
      (loop [index 0
             remaining path
             result {}]
            (if (seq remaining)
              (let [new-result (if-let [matched-point (points-set (first remaining))]
                                       (assoc result matched-point index)
                                       result)]
                   (recur (inc index) (rest remaining) new-result))
              result)))

(defn part-2 []
      (->> lines
           (map get-path)
           (map #(point->position intersections %))
           (apply merge-with +)
           vals
           (apply min)))

(defn -main
      [& args]
      (println (str "Part 1: " (part-1)))
      (println (str "Part 2: " (part-2))))