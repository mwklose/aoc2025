(ns aoc2025.day6 
  (:require
   [clojure.string :as string]))



(map (fn [x] (clojure.string/split x #"\s+")) (clojure.string/split-lines (slurp "input/day6_test.txt")))


(defn get-nth [start-at n coll] (
                                 take-nth n (drop start-at coll)
))

(get-nth 1 3 ["+" "*" "+" "0" "1" "2" "3" "4" "5" "6" "7" "8"])

(defn eval-infix [infixed] (eval (read-string (str "(" (clojure.string/join " " infixed) ")"))))

(eval-infix (get-nth 1 3 ["+" "*" "+" "0" "1" "2" "3" "4" "5" "6" "7" "8"]))

(defn d6p1 [filename] (let [input-lines (clojure.string/split-lines (slurp filename))
                            split-lines (map (fn [x] (clojure.string/split x #"\s+")) input-lines)
                            ncol (count (first split-lines))
                            filtered-lines  (map (fn [line]  (filter (fn [s] (not= s "")) line)) split-lines)
                            reverse-flat-lines (flatten (reverse filtered-lines))
                            ]

                        
(reduce + (
 map (fn [idx] (eval-infix (get-nth idx ncol reverse-flat-lines))) (range 0 ncol)
))
                        ))
(d6p1 "input/day6_test.txt")
(d6p1 "input/day6.txt")

;; Part 2 

;; Get as one long string (no \n)
(defn chars-to-number [char-seq] (
                                  let [
                                       joined-chars (clojure.string/join char-seq)
                                       replaced-chars (clojure.string/replace joined-chars #"\s" "")
                                       ]
                                   (if (= 0 (count replaced-chars)) nil (read-string replaced-chars))
                                   ))
(chars-to-number '(\1 \3 \2))
(chars-to-number '(1 \space \space))


(defn logical-all [logical-seq] (
                                 reduce #(and %1 %2) logical-seq
))

(logical-all '(true true true))
(logical-all '(true true false true))

(defn take-and-bake [nth-num operator-seq number-seq] (let [operator (nth operator-seq nth-num)
                                                            nums  (nth (filter #(not (logical-all (map nil? %))) (partition-by nil? number-seq)) nth-num)]
                                                        (if (= operator "+")
                                                          (apply + nums)
                                                          (apply * nums))))

(take-and-bake 2 '("+" "*" "+") '(1 2 3 nil 4 5 6 10 nil 4 7 8 9))

(defn d6p2 [filename] (let [slurp-lines (slurp filename)
                            digitcols (clojure.string/index-of slurp-lines "\n")
                            nrows (count (clojure.string/replace slurp-lines #"[^\n]" ""))
                            input-lines (clojure.string/replace slurp-lines "\n" "")
                            input-len (count input-lines)
                            operators (clojure.string/split (clojure.string/join (take-last digitcols input-lines)) #"\s+")
                            num-lines (take (- input-len digitcols) input-lines)
                            nums (map chars-to-number (map #(get-nth % digitcols num-lines) (range 0 digitcols)))
                            num-nils (count (filter nil? nums))
                            ]

                        (reduce + (map #(take-and-bake % operators nums) (range 0 (inc num-nils))))

                        ))

(d6p2 "input/day6_test.txt")
(d6p2 "input/day6.txt")