(:require [clojure.string/split-lines])


(defn l-r-to-number [lock-combo]
  (*
   (case (str (first lock-combo)) "L" -1 "R" 1)
   (Integer. (re-find #"\d+" lock-combo))))

(l-r-to-number "L45")
(l-r-to-number "R15")

;; Part 1 
(->> (slurp "input/day1.txt")
     (clojure.string/split-lines)
     (map l-r-to-number)
     (reductions +)
     (map (fn [x] (+ 50 x)))
     (filter (fn [x] (= 0 (mod x 100))))
     (count))








;; Part 2
(require '[clojure.math :as math])

(defn only-rh-turns [location movement]
  (cond
    ;;    If zero move after mod, then stay where you are
    (= movement 0) location
    ;;    If moving to the right, keep as normal 
    (> movement 0) (+ location movement)
    ;;    If moving to the left but not crossing a 0, keep as normal
    ;; (= (+ location movement) 0) (+ location movement 100)
    ;; Don't double count 0 crosses 
    (= (mod (+ location movement) 100) 0) (+ location movement 100)
    (= (mod location 100) 0) (+ location movement 100)
    (= (math/floor-div location 100) (math/floor-div (+ location movement) 100)) (+ location movement)
    ;;    By mod, can only cross a 0 once to the left.
    ;;    Adding  200 undoes going left, and adds one to the right, before landing on the mod-congruent value
    :else (+ location movement 200)))

(only-rh-turns 5 10)
(only-rh-turns 90 20)
(only-rh-turns 90 -20)
(only-rh-turns 110 -20)
(only-rh-turns 50 30)
(only-rh-turns 50 -50)
(only-rh-turns 100 -10)



(defn get-mod-turns  "For getting turns because L or R is over 100"
  [turns]
  (->> turns
       (map (fn [x] (abs (quot x 100))))
       (reduce + 0)))

(get-mod-turns [10 20 30])
(get-mod-turns [20 100 150])
(get-mod-turns [500 550 599])
(get-mod-turns [-150 100 45])

(defn reduce-as-rh-turns "Turns all turns into RH turns, the gets number of crosses"
  [turns]
  (quot
   (reduce only-rh-turns 50 (map (fn [x] (rem x 100)) turns))
   100))



(reduce only-rh-turns 50 (map (fn [x] (rem x 100)) test-turns))

(map (fn [x] (mod x 100)) test-turns)
(def turns (->> (slurp "input/day1.txt")
                (clojure.string/split-lines)
                (map l-r-to-number)))


(defn day1p2 [turns] (+
                       (get-mod-turns turns)
                       (reduce-as-rh-turns turns)))

(def test-turns (->> (slurp "input/day1_test.txt")
                     (clojure.string/split-lines)
                     (map l-r-to-number)))
(reduce-as-rh-turns turns)
(get-mod-turns turns)

(day1p2 turns)


