(ns aoc-2019.day11
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.math.numeric-tower :as math]))

(def line (->> "input-11.txt" io/resource io/reader slurp))
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
      1 :immediate
      2 :relative)))

(defn read-next-instruction [state]
  (let [code (-> state :code)
        instr-pointer (-> state :next-instruction)
        instr-code (code instr-pointer)]
    {:op-code (read-operation-code instr-code)
     :start instr-pointer
     :base (-> state :relative-base)
     :modes (vector
              (read-param-mode instr-code 1)
              (read-param-mode instr-code 2)
              (read-param-mode instr-code 3))}))

(defn get-arg [code instruction arg-index]
  (let [arg-slot-index (+ (instruction :start) arg-index)
        value-in-arg-slot (code arg-slot-index)
        arg-mode (-> instruction :modes (get ,,, (dec arg-index)))]
    (case arg-mode
      :immediate value-in-arg-slot
      :position (code value-in-arg-slot)
      :relative (code (+ (-> instruction :base) value-in-arg-slot)))))

(defn get-out-slot [code instruction arg-index]
  (let [arg-slot-index (+ (instruction :start) arg-index)
        value-in-arg-slot (code arg-slot-index)
        arg-mode (-> instruction :modes (get ,,, (dec arg-index)))]
    (if (= arg-mode :relative)
      (+ (-> instruction :base) value-in-arg-slot)
      value-in-arg-slot)))

(defn binary-operation [func instruction state]
  (let [code (-> state :code)
        instr-index (-> instruction :start)
        arg1 (get-arg code instruction 1)
        arg2 (get-arg code instruction 2)
        out-slot (get-out-slot code instruction 3)]
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
        out-slot (get-out-slot code instruction 1)]
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
        out-slot (get-out-slot code instruction 3)]
    (-> state
        (assoc-in ,,, [:code out-slot] (if (test arg1 arg2) 1 0))
        (assoc ,,, :next-instruction (+ instr-index 4)))))

(defn less-than [instruction state]
  (save-boolean #(< %1 %2) instruction state))

(defn equals [instruction state]
  (save-boolean #(= %1 %2) instruction state))

(defn update-relative-base [instruction state]
  (let [code (-> state :code)
        instr-index (-> instruction :start)
        arg (get-arg code instruction 1)
        current-base (-> instruction :base)]
    (-> state
        (assoc ,,, :relative-base (+ arg current-base))
        (assoc ,,, :next-instruction (+ instr-index 2)))))

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
                    9 update-relative-base
                    99 terminate})

(defn create-computer
  ([code]
   (create-computer code 100))
  ([code mem]
   {:code (apply conj code (into [] (take mem (repeat 0))))
    :next-instruction 0
    :relative-base 0
    :io {:in []
         :out []}
    :status :ready}))

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
        (recur result)))))

(defn scan-surface [surface position]
  (if-let [floor-color (surface position)] (list floor-color) '(0)))

(defn read-paint-instruction [computer surface position]
  (let [new-comp (->> (scan-surface surface position)
                      (feed-computer computer)
                      execute)
        paint-inst (-> new-comp :io :out)
        comp-with-reset-out (assoc-in new-comp [:io :out] [])]
    [paint-inst comp-with-reset-out]))

(defn paint [surface [position _] [target-color _]]
  (assoc surface position target-color))

(def directions {:up {0 :left 1 :right}
                 :down {0 :right 1 :left}
                 :left {0 :down 1 :up}
                 :right {0 :up 1 :down}})

(defn move-robot [[[x y] direction :as orientation] [_ left-right]]
  (if left-right
    (let [new-direction ((-> directions direction) left-right)]
      (case new-direction
        :up    [[x (inc y)] new-direction]
        :down  [[x (dec y)] new-direction]
        :left  [[(dec x) y] new-direction]
        :right [[(inc x) y] new-direction]))
    orientation))

(defn run-robot
  ([starting-tile-color]
   (run-robot (-> source-code (create-computer ,,, 10000))
              {[0 0] starting-tile-color}
              [[0 0] :up]))
  ([computer surface orientation]
   (if (= (-> computer :status) :terminated)
     surface
     (let [[paint-instr next-comp] (read-paint-instruction computer surface (first orientation))]
       (recur next-comp (paint surface orientation paint-instr) (move-robot orientation paint-instr))))))

(defn part-1 []
  (count (run-robot 0)))

(defn draw-line [start end y surface]
  (reduce (fn [line x]
            (if (->> [x y] surface (= 1))
              (str line "X")
              (str line " ")))
          ""
          (range start (inc end))))

(defn decr-range [current limit]
  (if (= current limit)
    (list current)
    (lazy-seq (cons current (decr-range (dec current) limit)))))

(defn visualize [surface]
  (let [painted (keys surface)
        min-x (->> painted (map first) (apply min))
        max-x (->> painted (map first) (apply max))
        min-y (->> painted (map second) (apply min))
        max-y (->> painted (map second) (apply max))]
    (->> (decr-range max-y min-y)
         (map #(draw-line min-x max-x % surface))
         (map println))))

(defn part-2 []
  (->> (run-robot 1)
       visualize))

(defn -main
  [& args]
  (println (str "Part 1: " (part-1)))
  (println (str "Part 2: " (part-2))))
