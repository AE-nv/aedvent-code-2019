(ns aoc-2019.day7
    (:require [clojure.java.io :as io]
      [clojure.string :as str]
      [clojure.math.numeric-tower :as math]
      [clojure.math.combinatorics :as combin]))

(def line (->> "input-7.txt" io/resource io/reader slurp))
(def source-code
  (->>
    (str/split line #",")
    (map read-string)
    (into [])))

(defn read-operation-code [instruction] (mod instruction 100))

(defn read-param-mode
      "Extracts the mode of the given parameter (1 based index) by applying a modifier (power of 10) and a mod operation"
      [instruction param-number]
      (let [modifier (math/expt 10 (inc param-number))]
           (case (mod (int (/ instruction modifier)) 10)
                 0 :position
                 1 :immediate)))

(defn read-next-instruction [state]
      (let [code (-> state :code)
            instr-pointer (-> state :next-instruction)
            instr-code (code instr-pointer)]
           {:op-code (read-operation-code instr-code)
            :start instr-pointer
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

(defn binary-operation [func instruction state]
      (let [code (-> state :code)
            instr-index (-> instruction :start)
            arg1 (get-arg code instruction 1)
            arg2 (get-arg code instruction 2)
            out-slot (-> (+ instr-index 3) code)]
           (-> state
               (assoc-in ,,, [:code out-slot] (func arg1 arg2))
               (assoc ,,, :next-instruction (+ instr-index 4)))))

(defn addition [instruction state]
      (binary-operation + instruction state))

(defn multiplication [instruction state]
      (binary-operation * instruction state))

(defn input [instruction state]
      (let [code (-> state :code)
            instr-index (-> instruction :start)
            [val-in & rest-in] (-> state :io :in)
            out-slot (-> (inc instr-index) code)]
           (if (nil? val-in)
             (assoc state :status :halted)
             (-> state
                 (assoc-in ,,, [:code out-slot] val-in)
                 (assoc-in ,,, [:io :in] rest-in)
                 (assoc ,,, :next-instruction (+ instr-index 2))))))

(defn output [instruction state]
      (let [code (-> state :code)
            instr-index (-> instruction :start)
            out (-> state :io :out)
            arg (get-arg code instruction 1)]
           (-> state
               (assoc-in ,,, [:io :out] (conj out arg))
               (assoc ,,, :next-instruction (+ instr-index 2)))))

(defn jump-if [predicate instruction state]
      (let [code (-> state :code)
            instr-index (-> instruction :start)
            arg1 (get-arg code instruction 1)
            arg2 (get-arg code instruction 2)]
           (if (predicate arg1)
             (assoc state :next-instruction arg2)
             (assoc state :next-instruction (+ instr-index 3)))))

(defn jump-if-true [instruction state]
      (jump-if #(not= % 0) instruction state))

(defn jump-if-false [instruction state]
      (jump-if #(= % 0) instruction state))

(defn save-boolean [test instruction state]
      (let [code (-> state :code)
            instr-index (-> instruction :start)
            arg1 (get-arg code instruction 1)
            arg2 (get-arg code instruction 2)
            out-slot (-> (+ instr-index 3) code)]
           (-> state
               (assoc-in ,,, [:code out-slot] (if (test arg1 arg2) 1 0))
               (assoc ,,, :next-instruction (+ instr-index 4)))))

(defn less-than [instruction state]
      (save-boolean #(< %1 %2) instruction state))

(defn equals [instruction state]
      (save-boolean #(= %1 %2) instruction state))

(defn terminate [instruction state]
      (assoc state :status :terminated))

(def operation-map {1 addition
                    2 multiplication
                    3 input
                    4 output
                    5 jump-if-true
                    6 jump-if-false
                    7 less-than
                    8 equals
                    99 terminate})

(defn create-computer [code]
      {:code code
       :next-instruction 0
       :io {:in []
            :out []}
       :status :ready})

(defn feed-computer [state inputs]
      (let [current-in (-> state :io :in)
            new-in (if (seq current-in) (apply conj current-in inputs) (apply vector inputs))]
           (-> state
               (assoc ,,, :status :ready)
               (assoc-in ,,, [:io :in] new-in))))

(defn execute
      [state]
      (let [status (-> state :status)
            instruction (read-next-instruction state)]
           (if (-> #{:halted :terminated} status)
             state
             (let [operation (-> instruction :op-code operation-map)
                   result (operation instruction state)]
                  (execute result)))))

(defn create-amp [intcode phase]
      (->
        (create-computer intcode)
        (feed-computer ,,, [phase])))

(defn amp-sequence-for [intcode phases]
      (->> phases
           (map #(create-amp intcode %))
           (apply vector)))

(defn run-single-amp-sequence [start-input amps]
      (reduce
        (fn [running-amps amp]
            (let [to-run (if (seq running-amps)
                           (feed-computer amp [(-> running-amps first :io :out last)])
                           (feed-computer amp [start-input]))]
                 (cons (execute to-run) running-amps)))
        '()
        amps))

(defn run-amp-sequence-loop [[last-amp & rest :as amps-reversed]]
      (let [output-last-amp (-> last-amp :io :out last)]
           (if (= (-> last-amp :status) :terminated)
             output-last-amp
             (run-amp-sequence-loop (run-single-amp-sequence output-last-amp (reverse amps-reversed))))))

(defn start [amps]
      (run-amp-sequence-loop (run-single-amp-sequence 0 amps)))

(defn run-phases [phase-generator]
      (->>
        phase-generator
        (map #(amp-sequence-for source-code %))
        (map start)
        (apply max)))

(defn part-1 []
      (run-phases (combin/permutations '(0 1 2 3 4))))

(defn part-2 []
      (run-phases (combin/permutations '(5 6 7 8 9))))

(defn -main
      [& args]
      (println (str "Part 1: " (part-1)))
      (println (str "Part 2: " (part-2))))
