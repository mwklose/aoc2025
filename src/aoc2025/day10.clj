(ns aoc2025.day10
  (:require
   [clojure.string :as string]
   [clojure.math :as math]
   [clojure.math.combinatorics :as combo]
   [clojure.core.logic :as logic]
   [clojure.core.logic.fd :as fd]))


(defn recursive-button-solver [target n buttons] (let [button-combinations (combo/combinations buttons n)
                                                       indicator-after-buttons (map #(reduce bit-xor 0 %) button-combinations)
                                                       match-within-n (reduce (fn [acc, x] (or acc (= x target))) false indicator-after-buttons)]
                                                   (cond
                                                     match-within-n n
                                                     :else (recursive-button-solver target (inc n) buttons))))

(defn button-to-bit-helper [length button-bit set-idx] (bit-set button-bit (- (dec length) set-idx)))
(button-to-bit-helper 4 0 3)

(defn button-to-bit [button-s num-indicators] (let [trim-button (clojure.string/replace button-s #"\(|\)" "")
                                                    button-num (map read-string (clojure.string/split trim-button #","))
                                                    partial-button-reduce (partial button-to-bit-helper num-indicators)]
                                                (reduce partial-button-reduce 0 button-num)))

(button-to-bit "(1,2)" 3)

(defn indicator-to-bit [indicator idx bit]
  (cond
    (>= idx (count indicator)) bit
    (= (subs indicator idx (inc idx)) ".") (indicator-to-bit indicator (inc idx) (bit-shift-left bit 1))
    (= (subs indicator idx (inc idx)) "#") (indicator-to-bit indicator (inc idx) (bit-or (bit-shift-left bit 1) 2r1))))

;;    (indicator-to-bit indicator (inc idx) (bit-set bit idx)))))

(indicator-to-bit ".##." 0 2r0)
(indicator-to-bit "#.#.#" 0 2r0)

(defn factory-solver [input-line] (let [[_ indicator] (re-find #"\[(.*)\]" input-line)
                                        bit-indicator (indicator-to-bit indicator 0 2r0)
                                        [_ buttons _] (re-matches #"\[.*\] ((\([\d,]+\)\s*)+) \{.*\}" input-line)
                                        split-buttons (map #(button-to-bit % (count indicator)) (clojure.string/split buttons #"\s"))]

                                    (recursive-button-solver bit-indicator 1 split-buttons)))


(defn d10p1 [filename] (let [input-lines (clojure.string/split-lines (slurp filename))]
                         (reduce + (map factory-solver input-lines))))

(d10p1 "input/day10_test.txt")
(d10p1 "input/day10.txt")

;; Part 2
;; This feels like an NP-complete problem 

(defn reduce-target-helper [joltage-target button times]
  (mapv (fn [idx] (if (some #(= idx %) button)
                    (- (get joltage-target idx) times)
                    (get joltage-target idx)))
        (range (count joltage-target))))
  
(reduce-target-helper [3 5 4 7] '(1 3) 4)


(defn recursive-joltage-solver [joltage-target buttons button-presses total-count level] (let [first-button (first buttons)
                                                                                               remaining-buttons (rest buttons)
                                                                                               new-target (reduce-target-helper joltage-target first-button button-presses)]
                                                                                           (println "js" level button-presses total-count new-target first-button remaining-buttons)

                                                                                          ;;  TODO: try maximum number of buttons presses
                                                                                          ;;  Recurse inwards
                                                                                          ;;  If solution, return 
                                                                                          ;;  If no solution, try next number of buttons



                                                                                           (cond

                                                                                             (nil? first-button) nil
                                                                                             (nil? remaining-buttons) nil
                                                                                             (= (count remaining-buttons) 0) nil
                                                                                             (< 0 button-presses) (recursive-joltage-solver joltage-target remaining-buttons (apply min (map #(get joltage-target %) (first remaining-buttons))) total-count (inc level))
                                                                                             (= new-target (reduce into (repeat (count new-target) 0))) total-count
                                                                                             
                                                                                             :else (let [recursive-call  (recursive-joltage-solver
                                                                                                                          new-target
                                                                                                                          remaining-buttons
                                                                                                                          (+ total-count button-presses)
                                                                                                                          (apply min (map #(get joltage-target %) (first remaining-buttons)))
                                                                                                                          (inc level))]
                                                                                                     (if (nil? recursive-call) (recursive-joltage-solver joltage-target buttons (dec button-presses) total-count level) recursive-call)))))
                                                                                             
                                                                                           


                           


(defn joltage-solver [input-line] (let [[_ buttons _] (re-matches #"\[.*\] ((\([\d,]+\)\s*)+) \{.*\}" input-line)
                                        split-buttons (mapv #(map read-string %) (map #(clojure.string/split % #",") (map #(clojure.string/replace % #"\(|\)" "") (clojure.string/split buttons #"\s"))))
                                        ordered-split-buttons (sort-by count > split-buttons)
                                        [_ _ joltages-s] (re-find #"\[.*\] (\(.*\))* \{(.*)\}" input-line)
                                        joltage-target (mapv read-string (clojure.string/split joltages-s #","))
                                        ;; joltage-buttons (mapcat (partial get-max-repeats joltage-target) split-buttons)
                                        ]
                                    (println "solver" joltage-target ordered-split-buttons)
                                    (recursive-joltage-solver joltage-target ordered-split-buttons (apply min (map #(get joltage-target %) (first ordered-split-buttons))) 0 0)
                                    ;; (recursive-joltage-solver joltage-target min-repeats joltage-buttons)
                                    ))

(defn d10p2 [filename] (let [input-lines (clojure.string/split-lines (slurp filename))
                             joltage-solutions (map joltage-solver (take 3 input-lines))
                             ]
                         (println "answer" joltage-solutions)
                         (reduce + joltage-solutions)
                         ))

(d10p2 "input/day10_test.txt")
(d10p2 "input/day10.txt")


;; Need to solve with Linear Programming
;; Or brute force...but be smart about it