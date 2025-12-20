(ns aoc2025.day3
  (:require
   [clojure.string :as string]))

(defn read-input-lines [filename] (clojure.string/split-lines (slurp filename)))

(read-input-lines "input/day3_test.txt")

(defn line-str-to-list [linestrs] (map (fn [line] (
                                                   map (fn [c] (read-string c)) (clojure.string/split line #"")
                                                   
                                                   )) linestrs))


(let [split-values (clojure.string/split "123435" #"")]
  (map (fn [x] (read-string x)) split-values))


(defn recursive-joltage [bank acc target-value length] (cond
                                                         ;;   If at capacity, return 
                                                         (= length 0) acc
                                                         (= (count bank) 0) acc
                                                         (contains? (set bank) target-value) (apply max (map
                                                                                                                        (fn [x] (let [[first-half second-half] (split-at (+ 1 (.indexOf bank target-value)) bank)]
                                                                                                                                  (recursive-joltage
                                                                                                                                   second-half
                                                                                                                                  (+ (* 10 acc) target-value)
                                                                                                                                   x
                                                                                                                                   (- length 1))))
                                                                                                                        (distinct bank)))
                                                         :else 0))
(recursive-joltage '() 0 1 0)
(recursive-joltage '() 55 1 1)
(recursive-joltage (vector 1 2 3) 0 2 2)
(recursive-joltage (vector 9 8 7 6 5 4 3 2 1 1 1) 0 9 2)
(recursive-joltage '(8 1 1 1 9) 0 8 2)
(recursive-joltage '(2 3 4 2 3 4 2 3 4 2 3 4 2 7 8) 0 7 2)

(defn max-recursive-joltage [bank length] (
                                           apply max (map (fn [x] (recursive-joltage bank 0 x length)) (range 1 10))
))

(max-recursive-joltage '(8 1 1 1 9) 2)

(defn d3p1 [filename] (reduce + (map (fn [x] (max-recursive-joltage x 2)) (line-str-to-list (read-input-lines filename)))))

(d3p1 "input/day3_test.txt")
(d3p1 "input/day3.txt")


;; Part 2
;; max-recursive-joltage not working from P1 

(defn recursive-joltage-better [bank acc target-value length] (cond
                                                                ;;   If at capacity, return
                                                                (= length 0) acc
                                                                (< target-value 0) 0
                                                                (< (count bank) length) 0
                                                                (= (count bank) 0) 0
                                                                (contains? (set bank) target-value) (let [[first-half second-half] (split-at (+ 1 (.indexOf bank target-value)) bank)
                                                                                                          result (recursive-joltage-better
                                                                                                                  second-half
                                                                                                                  (+ (* 10 acc) target-value)
                                                                                                                  9
                                                                                                                  (- length 1))] (if (= result 0)
                                                                                                                                   (recursive-joltage-better bank acc (- target-value 1) length)
                                                                                                                                   result))
                                                                                                      
                                                                :else (recursive-joltage-better bank acc (- target-value 1) length)))
(recursive-joltage-better '() 0 1 0)
(recursive-joltage-better '() 55 1 1)
(recursive-joltage-better (vector 1 2 3) 0 2 2)
(recursive-joltage-better (vector 9 8 7 6 5 4 3 2 1 1 1) 0 9 10)
(recursive-joltage-better '(8 1 1 1 9) 0 8 5)
(recursive-joltage-better '(2 3 4 2 3 4 2 3 4 2 3 4 2 7 8) 0 9 21)

(defn max-recursive-joltage-better [bank length] (let [result (recursive-joltage-better bank 0 9 length)] (cond
                                                                                                            (< length 0) 0
                                                                                                            (= 0 (result)) 
                                                                                                            
                                                                                                            )
                                                      apply max (map (fn [x] (recursive-joltage bank 0 x length)) (range 1 10))))


(defn d3p2 [filename] (
                       reduce 
                       + 
                       (map 
                        (fn [x] (
                                 recursive-joltage-better 
                                 x 
                                 0 
                                 9 
                                 12
                                 )) 
                        (line-str-to-list (
                                           read-input-lines filename)
                                          ))))

(d3p2 "input/day3_test.txt")
(d3p2 "input/day3.txt")
(def d3p2-ans (d3p2 "input/day3.txt"))
d3p2-ans