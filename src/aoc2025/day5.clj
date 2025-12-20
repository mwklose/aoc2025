(ns aoc2025.day5
  (:require
   [clojure.string :as string]))


(defn input-to-fresh-ranges [filename] (let [input-list (clojure.string/split (slurp filename) #"\n\n")
                                             fresh-ranges (get input-list 0)
                                             ingredient-list (get input-list 1)]
                                         fresh-ranges))
(defn input-to-ingredient-list [filename] (let [input-list (clojure.string/split (slurp filename) #"\n\n")
                                                fresh-ranges (get input-list 0)
                                                ingredient-list (get input-list 1)]
                                            (clojure.string/split ingredient-list #"\n")))

(input-to-fresh-ranges "input/day5_test.txt")
(input-to-ingredient-list "input/day5_test.txt")

(defn or-pred [pred-list] (reduce (fn [x pred] (or x pred)) false pred-list))


(defn ingredient-in-range [ingredient-str fresh-range] (let [ing-range (clojure.string/split fresh-range #"\-")
                                                             low-range (read-string (get ing-range 0))
                                                             high-range (read-string (get ing-range 1))
                                                             ingredient (read-string ingredient-str)]
                                                         (and (>= ingredient low-range) (<= ingredient high-range))))
(ingredient-in-range "1" "2-5")
(ingredient-in-range "1" "1-5")
(ingredient-in-range "7" "2-5")
(ingredient-in-range "-1" "2-5")

(defn ingredient-in-any-range [ingredient fresh-ranges] (let [ing-ranges (clojure.string/split fresh-ranges #"\n")]
                                                          (or-pred (map (fn [x] (ingredient-in-range ingredient x)) ing-ranges))))
(ingredient-in-any-range "1" "2-5\n1-3")
(ingredient-in-any-range "1" "2-5\n9-12")


(defn d5p1 [filename] (let [fresh-ranges (input-to-fresh-ranges filename)
                            ingredients (input-to-ingredient-list filename)]

                        (count (filter (fn [x] (ingredient-in-any-range x fresh-ranges)) ingredients))))
(d5p1 "input/day5_test.txt")
(d5p1 "input/day5.txt")


;; Part 2

;; This is just sets, but run out of memory if do so

(clojure.string/join ";"  ["123"])
(defn add-fresh-range-to-set [existing-set fresh-range] (let [ing-range (clojure.string/split fresh-range #"\-")
                                                              low-range (read-string (get ing-range 0))
                                                              high-range (read-string (get ing-range 1))]

                                                          (clojure.set/union existing-set (set (range low-range (+ high-range 1))))))

(add-fresh-range-to-set (set [7 1 5]) "1-5")



(input-to-fresh-ranges "input/day5_test2.txt")

(defn combine-intervals [interval1 interval2] (let [i1s (clojure.string/split interval1 #";")
                                                    i1 (clojure.string/split interval1 #"\-")
                                                    i1-low (read-string (get i1 0))
                                                    i1-high (read-string (get i1 1))
                                                    i2 (clojure.string/split interval2 #"\-")
                                                    i2-low (read-string (get i2 0))
                                                    i2-high (read-string (get i2 1))]




                                                (cond

                                                  (and (<= i2-low i1-high) (<= i1-low i2-high)) (str (min i1-low i2-low) "-" (max i1-high i2-high))
                                                  :else (str interval1 ";" interval2))))
(combine-intervals "1-5" "5-10")




(defn combine-sorted-intervals [interval1 interval2] (let [split-interval (clojure.string/split interval1 #";")
                                                           first-intervals (take (- (count split-interval) 1) split-interval)
                                                           final-interval (take-last 1 split-interval)
                                                           final-interval-vec (clojure.string/split (first final-interval) #"\-")
                                                           final-interval-low (read-string (get final-interval-vec 0))
                                                           final-interval-high (read-string (get final-interval-vec 1))
                                                           interval2-vec (clojure.string/split interval2 #"\-")
                                                           interval2-low (read-string (get interval2-vec 0))
                                                           interval2-high (read-string (get interval2-vec 1))]


                                                       (cond

                                                         (and (<= final-interval-low interval2-high) (<= interval2-low final-interval-high))
                                                         (str (clojure.string/join ";" first-intervals) (if (> (count first-intervals) 0 ) ";" "") (min final-interval-low interval2-low) "-" (max final-interval-high interval2-high))


                                                         :else (str interval1 ";" interval2))))
(combine-sorted-intervals "1-5;7-10;14-16" "23-25")
(combine-sorted-intervals "1-5;7-10;14-18" "15-17")
(combine-sorted-intervals "1-5" "7-10")

(defn interval-to-size [interval-str] (let [interval-vec (clojure.string/split interval-str #"\-")
                                            interval-low (read-string (get interval-vec 0))
                                            interval-high (read-string (get interval-vec 1))]
                                        (+ 1 (- interval-high interval-low))))




(defn d5p2 [filename] (let [fresh-ranges (clojure.string/split (input-to-fresh-ranges filename) #"\n")
                            sorted-ranges (sort-by (fn [x] (let
                                                            [interval (clojure.string/split x #"\-")]
                                                             (read-string (get interval 0))))

                                                   < fresh-ranges)
                            first-sorted-range  (first sorted-ranges)]


                               (reduce + (map interval-to-size (clojure.string/split (reduce combine-sorted-intervals first-sorted-range sorted-ranges) #";"))
                              )))
(d5p2 "input/day5_test.txt")
(d5p2 "input/day5.txt")
(d5p2 "input/day5_test2.txt")