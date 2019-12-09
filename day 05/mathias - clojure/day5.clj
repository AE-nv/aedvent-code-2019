(ns aoc-2019.day5
    (:require [clojure.java.io :as io]
      [clojure.string :as str]
      [clojure.math.numeric-tower :as math]))

(def line (->> "input-5.txt" io/resource io/reader slurp))
(def code
  (->>
    (str/split line #",")
    (map read-string)
    (into [])))

(def given-input-value 5)

(defn read-operation-code [instruction] (mod instruction 100))
(defn read-param-mode
      "Extracts the mode of the given parameter (1 based index) by applying a modifier (power of 10) and a mod operation"
      [instruction param-number]
      (let [modifier (math/expt 10 (inc param-number))]
           (case (mod (int (/ instruction modifier)) 10)
                 0 :position
                 1 :immediate)))
(defn read-instruction [code index]
      (let [instr-code (code index)]
           {:op-code (read-operation-code instr-code)
            :start index
            :modes (vector
                     (read-param-mode instr-code 1)
                     (read-param-mode instr-code 2)
                     (read-param-mode instr-code 3))}))
(defn get-arg [code instruction arg-index]
      (let [arg-slot-index (+ (instruction :start) arg-index)
            value-in-arg-slot (code arg-slot-index)
            arg-mode (-> instruction :modes (get ,,, (dec arg-index)))]
           (if (= arg-mode :immediate)
             value-in-arg-slot
             (code value-in-arg-slot))))

(defn addition [code instruction]
      (let [instr-index (-> instruction :start)
            arg1 (get-arg code instruction 1)
            arg2 (get-arg code instruction 2)]
           {:output-to (-> (+ instr-index 3) code)
            :next-instruction (+ instr-index 4)
            :result (+ arg1 arg2)}))

(defn multiplication [code instruction]
      (let [instr-index (-> instruction :start)
            arg1 (get-arg code instruction 1)
            arg2 (get-arg code instruction 2)]
           {:output-to (-> (+ instr-index 3) code)
            :next-instruction (+ instr-index 4)
            :result (* arg1 arg2)}))

(defn input [code instruction]
      (let [instr-index (-> instruction :start)]
           {:output-to (-> (inc instr-index) code)
            :next-instruction (+ instr-index 2)
            :result given-input-value}))

(defn output [code instruction]
      (let [instr-index (-> instruction :start)
            arg (get-arg code instruction 1)]
           (println (str "output: " arg))
           {:next-instruction (+ instr-index 2)}))

(defn jump-if [code instruction predicate]
      (let [instr-index (-> instruction :start)
            arg1 (get-arg code instruction 1)
            arg2 (get-arg code instruction 2)]
           (if (predicate arg1)
             {:next-instruction arg2}
             {:next-instruction (+ instr-index 3)})))

(defn jump-if-true [code instruction]
      (jump-if code instruction #(not= % 0)))

(defn jump-if-false [code instruction]
      (jump-if code instruction #(= % 0)))

(defn save-boolean [code instruction test]
      (let [instr-index (-> instruction :start)
            arg1 (get-arg code instruction 1)
            arg2 (get-arg code instruction 2)]
           {:output-to (-> (+ instr-index 3) code)
            :next-instruction (+ instr-index 4)
            :result (if (test arg1 arg2) 1 0)}))

(defn less-than [code instruction]
      (save-boolean code instruction #(< %1 %2)))

(defn equals [code instruction]
      (save-boolean code instruction #(= %1 %2)))

(def operation-map {1 addition
                    2 multiplication
                    3 input
                    4 output
                    5 jump-if-true
                    6 jump-if-false
                    7 less-than
                    8 equals})

(defn execute
      ([code]
       (execute code 0))
      ([code index]
       (let [op-code (get code index)]
            (if (= 99 op-code)
              (first code)
              (let [instruction (read-instruction code index)
                    operation (-> instruction :op-code operation-map)
                    exec-result (operation code instruction)
                    next-instr-slot (-> exec-result :next-instruction)
                    output-slot (-> exec-result :output-to)
                    result (-> exec-result :result)]
                   (execute
                     (if result
                       (assoc code output-slot result)
                       code)
                     next-instr-slot))))))

(defn -main
      [& args]
      ;; don't for get to use the correct input 1 for part1, 5 for part 2
      (println (execute code)))