(ns aoc2025.day8
  (:require
   [clojure.string :as string]
   [clojure.math :as math]
   [clojure.math.combinatorics :as combo]
   [clojure.set :as set]))


;; Read input

;; Keep first 1000 distances with vertices

(defn sqdist [p1 p2] (reduce + (map (fn [x] (clojure.math/pow (apply - x) 2)) (partition 2 (interleave p1 p2)))))

(sqdist '(1 2 3) '(4 5 6))

(defn box-to-coords [box] (map read-string (clojure.string/split box #",")))

(box-to-coords "123,123,456")

(defn box-to-distances [boxes] (let [[box1 box2] boxes
                                     b1 (box-to-coords box1)
                                     b2 (box-to-coords box2)] (sqdist b1 b2)))

(box-to-distances '("1,2,3" "4,5,6"))


;; TODO - this is not right implementation. Skip boxes already in circuit


(defn sort-boxes-by-distance [lines] (
                                        sort-by box-to-distances (combo/combinations lines 2)))
                                        



(let [
      lines (clojure.string/split-lines (slurp "input/day8.txt"))
      ] (count (map box-to-distances (sort-boxes-by-distance lines))))


(defn index-of-contains [item vec-sets] (
                                         first (filter #(if (contains? (get vec-sets %) item) % nil) (range (count vec-sets))))
)

(index-of-contains 7 [#{1 2 3} #{4 5 6}])

(index-of-contains "3,5,7" [#{"1,2,3"} #{"3,5,7"} #{"56,67,78"}])


(index-of-contains "805,96,715" [#{"57,618,57"} #{"906,360,560"} #{"592,479,940"} #{"352,342,300"} #{"466,668,158"} #{"542,29,236"} #{"431,825,988 425,690,689 162,817,812"} #{"739,650,466"} #{"52,470,668"} #{"216,146,977"} #{"819,987,18"} #{"117,168,530"} #{"805,96,715"} #{"346,949,466"} #{"970,615,88"} #{"941,993,340"} #{"862,61,35"} #{"984,92,344"}])

(defn add-item-vecset-index [item vec-sets index] (
                                                   mapv (fn [idx] (if (= index idx) (conj (get vec-sets index) item) (get vec-sets idx))) (range (count vec-sets))  
))

(add-item-vecset-index 7 [#{1 2 3} #{4 5 6}] 0)

(concat [#{1 2 3} #{4 5 6}] [#{8 9}])

(defn merge-circuits [vec-sets circuit1 circuit2] (let [mincircuit (min circuit1 circuit2)
                                                        maxcircuit (max circuit1 circuit2)
                                                        is-min-first (= mincircuit 0)
                                                        is-max-last (= maxcircuit (dec (count vec-sets)))
                                                        union-set (set/union (get vec-sets circuit1) (get vec-sets circuit2))
                                                        middle-region (subvec vec-sets (inc mincircuit) maxcircuit)]
                                                    (cond
                                                      (and is-min-first is-max-last) (conj middle-region union-set)
                                                      is-min-first (into [] (concat middle-region (subvec vec-sets (inc maxcircuit)) (vector union-set)))
                                                      is-max-last (into [] (concat (subvec vec-sets 0 mincircuit) (vector union-set) middle-region))
                                                      :else (into [] (concat (subvec vec-sets 0 mincircuit) (vector union-set) middle-region (subvec vec-sets (inc maxcircuit))))
                                                      )
                                                    ))


(vec (map #(set [%]) (clojure.string/split-lines (slurp "input/day8_test.txt"))))

(defn construct-circuits [lines n-init ] (let [sorted-box-pairs (sort-boxes-by-distance lines)
                                               initial-circuits (vec (map #(set [%]) lines))
                                               ]

                                           (loop [n n-init
                                                  box-pairs sorted-box-pairs
                                                  circuits initial-circuits]

                                             (if (> n 0)
                                               (let [first-pair (first box-pairs)
                                                     [first-item next-item] first-pair
                                                     rest-pair (rest box-pairs)
                                                     first-item-index (index-of-contains first-item circuits)
                                                     next-item-index (index-of-contains next-item circuits)]
                                                 (println n first-item first-item-index next-item next-item-index (map count (take 3 (sort-by count > circuits))))
                                                 ;;    Case 1: neither in any circuits 
                                                 ;;    Case 2: only item 1 in a circuit 
                                                 ;;    Case 3: only item 2 in a circuit
                                                 ;;    Case 4: item1 and item 2 in separate circuits  -> merge
                                                 ;;    Case 5: item 1 and item 2 in same circuit -> move to next entry
                                                 (cond
                                                   (and (nil? first-item-index) (nil? next-item-index)) (recur (dec n) rest-pair (conj circuits #{first-item next-item}))
                                                   (nil? first-item-index) (recur (dec n) rest-pair (add-item-vecset-index first-item circuits next-item-index))
                                                   (nil? next-item-index) (recur (dec n) rest-pair (add-item-vecset-index next-item circuits first-item-index))
                                                   (not= first-item-index next-item-index) (recur (dec n) rest-pair (merge-circuits circuits first-item-index next-item-index))
                                                   :else (recur (dec n) rest-pair circuits)))
                                               circuits))))

(construct-circuits (clojure.string/split-lines (slurp "input/day8_test.txt")) 10)

(defn d8p1 [filename takenum] (let [coords (slurp filename)
                                    lines (clojure.string/split-lines coords)]
                              (reduce * (take 3 (map count (sort-by count > (construct-circuits lines takenum)))))))


(d8p1 "input/day8_test.txt" 10)
(d8p1 "input/day8.txt" 1000)

;; Part 2
(defn exhaust-circuts [lines n-init] (let [sorted-box-pairs (sort-boxes-by-distance lines)
                                           initial-circuits (vec (map #(set [%]) lines))
                                           [prior-first prior-next] (first sorted-box-pairs)
                                           ]

                                       (loop [n n-init
                                              box-pairs sorted-box-pairs
                                              circuits initial-circuits
                                              previous-first prior-first
                                              previous-next prior-next
                                              ]

                                         (if (> n 0)
                                           (let [first-pair (first box-pairs)
                                                 [first-item next-item] first-pair
                                                 rest-pair (rest box-pairs)
                                                 first-item-index (index-of-contains first-item circuits)
                                                 next-item-index (index-of-contains next-item circuits)]
                                             (println n (count circuits) first-item first-item-index next-item next-item-index (map count (take 3 (sort-by count > circuits))))
                                             ;;    Case 1: neither in any circuits 
                                             ;;    Case 2: only item 1 in a circuit 
                                             ;;    Case 3: only item 2 in a circuit
                                             ;;    Case 4: item1 and item 2 in separate circuits  -> merge
                                             ;;    Case 5: item 1 and item 2 in same circuit -> move to next entry
                                             (cond
                                               (= (count circuits) 1) (str previous-first ";" previous-next)
                                               (and (nil? first-item-index) (nil? next-item-index)) (recur (dec n) rest-pair (conj circuits #{first-item next-item}) first-item next-item)
                                               (nil? first-item-index) (recur (dec n) rest-pair (add-item-vecset-index first-item circuits next-item-index) first-item next-item)
                                               (nil? next-item-index) (recur (dec n) rest-pair (add-item-vecset-index next-item circuits first-item-index) first-item next-item)
                                               (not= first-item-index next-item-index) (recur (dec n) rest-pair (merge-circuits circuits first-item-index next-item-index) first-item next-item)
                                               :else (recur n rest-pair circuits first-item next-item)))
                                           circuits))))


(defn d8p2 [filename takenum] (let [coords (slurp filename)
                                    lines (clojure.string/split-lines coords)]
                                 (exhaust-circuts lines takenum)))

(d8p2 "input/day8_test.txt" 5000)
(d8p2 "input/day8.txt" 5000000)
(* 4360 9644)