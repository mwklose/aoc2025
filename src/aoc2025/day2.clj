(ns aoc2025.day2
  (:require
   [clojure.string :as string]
   [clojure.math :as math]))

(defn duplicate-x [x] (read-string (str x x)))
(defn count-repeated-in-range [lowbound highbound x] (if (and
                                                          (>= (duplicate-x x) lowbound)
                                                          (<= (duplicate-x x) highbound))
                                                       (duplicate-x x)
                                                       0))
(defn range-from-bound [range-int] (
                                       let [
                                            num_digits (int (clojure.math/ceil (clojure.math/log10 range-int)))
                                            is_even (= (mod num_digits 2) 0)
                                            subs_size (quot num_digits 2)
                                            ]
                                   (if is_even (read-string (subs (str range-int) 0 subs_size))
                                           (int (clojure.math/pow 10 subs_size))
                                          
                                     )))

(range-from-bound 95)
(range-from-bound 998)
(range-from-bound 1012)
(range-from-bound 115)
(range-from-bound  1188511880 )

(defn sum-repeated-in-range [lowbound highbound] (reduce + (map (fn [x] (count-repeated-in-range lowbound highbound x)) (range (range-from-bound lowbound) (+ 1 (range-from-bound highbound))))))

(sum-repeated-in-range 11 22)
(sum-repeated-in-range 95 115)
(sum-repeated-in-range 998 1012 )
(sum-repeated-in-range 1188511880 1188511890 )



(defn range-to-palindrome [range_str] ( 
  let [
       range-vec (clojure.string/split range_str #"\-")
       lowbound (read-string (get range-vec 0))
       highbound (read-string (get range-vec 1))
       ] 
    (sum-repeated-in-range lowbound highbound)
  ;;   TODO: need to first figure out how to turn STR into INT.
  ))

(range-to-palindrome "95-113")


(defn d2p1 [filename] (
  reduce + (
map (fn [x] (range-to-palindrome x))
     (clojure.string/split (slurp filename) #",")
  )                     
))


(d2p1 "input/day2_test.txt")
(d2p1 "input/day2.txt")


;; Part 2
(defn replicate-x [x reps] (read-string (apply str (repeat reps x))))
(replicate-x 56 4 )

(defn is-replicated [x] (let [regex-matches (re-find #"^(\d+)(\1)+$" (str x))]
                          (if (nil? regex-matches) 0 x)))

(is-replicated 38593859)

(defn range-to-repeated [x] (let [range-vec (clojure.string/split x #"\-")
                                  lowbound (read-string (get range-vec 0))
                                  highbound (read-string (get range-vec 1))]
                              (reduce + (map is-replicated (range lowbound (+ highbound 1))))))

(range-to-repeated "95-113")

(defn d2p2 [filename] (reduce + (map (fn [x] (range-to-repeated x))
                                     (clojure.string/split (slurp filename) #","))))

(d2p2 "input/day2_test.txt")
(d2p2 "input/day2.txt")