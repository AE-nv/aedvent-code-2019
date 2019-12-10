(ns aoc-2019.day6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-orbit [orbit-str]
  (let [pair (str/split orbit-str #"\)")]
    {:center (first pair) :satelite (second pair)}))

(def all-orbits (->> "input-6.txt" io/resource io/reader line-seq
                     (map parse-orbit)))

(defn find-orbit-of [object orbits]
  (->> orbits
       (some (fn [{satelite :satelite :as orbit}] (if (= satelite object) orbit)))))

(defn count-orbits-for [object orbits]
  (let [center-of-object (-> (find-orbit-of object orbits) :center)]
    (if center-of-object
      (inc (count-orbits-for center-of-object orbits))
      0)))

(defn part-1 []
  (let [all-orbiting (->> all-orbits (map :satelite))]
    (->> all-orbiting
         (map #(count-orbits-for % all-orbits))
         (reduce + 0))))

(defn get-path-to-center [object orbits]
  (let [center-of-object (-> (find-orbit-of object orbits) :center)]
    (if center-of-object
      (cons center-of-object (get-path-to-center center-of-object orbits))
      '())))

(defn part-2 []
  (let [my-path-to-center (->> (get-path-to-center "YOU" all-orbits) (into #{}))
        santas-path-to-center (->> (get-path-to-center "SAN" all-orbits) (into #{}))]
    (+ (-> (set/difference my-path-to-center santas-path-to-center) count)
       (-> (set/difference santas-path-to-center my-path-to-center) count))))

(defn -main
  [& args]
  (println (str "Part 1: " (part-1)))
  (println (str "Part 2: " (part-2))))