(ns aoc2025.day9
  (:require
   [clojure.string :as string]
   [clojure.math :as math]
   [clojure.math.combinatorics :as combo]
   ))


(defn combo-to-area [corners] (
                               let [[c1 c2] corners
                                    [x1 y1] (map read-string (clojure.string/split c1 #","))
                                    [x2 y2] (map read-string (clojure.string/split c2 #",")) 
                                    ]
                                (* (inc (abs (- x2 x1))) (inc (abs (- y2 y1))))
                                ))


(defn d9p1 [filename] (
                       let [
                            points (clojure.string/split-lines (slurp filename))
                            combos (combo/combinations points 2)
                       ]
                      (apply max (map combo-to-area combos))
))

(d9p1 "input/day9_test.txt")
(d9p1 "input/day9.txt")


;; Part 2

;; TODO: handle current error

(defn ccw [a b c] (let [[ax ay] a
                        [bx by] b
                        [cx cy] c
                        ad (* (- bx ax) (- cy ay))
                        bc (* (- cx ax) (- by ay))
                        det (- ad bc)]
                    (cond
                      (< det 0) -1
                      (> det 0) 1
                      :else 0)))

(defn lines-intersect [point1 point2 test-1 test-2] (let [p12t1 (ccw point1 point2 test-1)
                                                          p12t2 (ccw point1 point2 test-2)
                                                          t12p1 (ccw test-1 test-2 point1)
                                                          t12p2 (ccw test-1 test-2 point2)
                                                          p-straight (or (= p12t1 0) (= p12t2 0))
                                                          t-straight (or (= t12p1 0) (= t12p2 0))
                                                          ]
                                                      (cond
                                                        (and p-straight (not= t12p1 t12p2)) 0
                                                        (and t-straight (not= p12t1 p12t2)) 0
                                                        (and (not= p12t1 p12t2) (not= t12p1 t12p2)) 1
                                                        :else 0)))

(lines-intersect '(1 1) '(10 1) '(5 5) '(5 -5))
(lines-intersect '(5 5) '(5 -5) '(1 1) '(10 1))
(lines-intersect '(5 5) '(5 -5) '(10 1) '(1 1))
(lines-intersect '(1 1) '(10 1) '(5 5) '(5 1))
(lines-intersect '(1 1) '(10 1) '(10 5) '(10 1))
(lines-intersect '(10 1) '(1 1) '(10 5) '(10 1))
(lines-intersect '(1 1) '(7 1) '(10 5) '(10 1))
(lines-intersect '(1 1) '(3 3) '(5 5) '(7 1))


(defn check-crossings [point1 point2 other-points] (int (reduce + (map #(let [[t1 t2] %] (lines-intersect point1 point2 t1 t2)) (partition 2 1 (take (inc (count other-points)) (cycle other-points)))))))

(let [points-s (clojure.string/split-lines (slurp "input/day9_test.txt"))
                            points (map #(map read-string (clojure.string/split % #",")) points-s)
                            ]
                        (check-crossings '(2 3) '(9 5) points)
)

;; TODO: need to figure out valid rectangle ornot 
(defn valid-rectangle [point1 diag other-points] (let [[x1 y1] point1
                                                       [x2 y2] diag
                                                       ccw-test-point (list x2 y1)
                                                       ccw-test (ccw point1 diag ccw-test-point)
                                                       point2 (if (pos? ccw-test) (list x1 y2) (list x2 y1))
                                                       point4 (if (pos? ccw-test) (list x2 y1) (list x1 y2))
                                                       point2-polarity (check-crossings point1 point2 other-points)
                                                       point3-polarity (check-crossings point2 diag other-points)
                                                       point4-polarity (check-crossings diag point4 other-points)
                                                       point1-polarity (check-crossings point4 point1 other-points)

                                                       minx (min x1 x2)
                                                       maxx (max x1 x2)
                                                       miny (min y1 y2)
                                                       maxy (max y1 y2)
                                                       area (* (inc (- maxx minx)) (inc (- maxy miny)))]
                                                   (println point1 point2 diag point4  point2-polarity point3-polarity point4-polarity point1-polarity
                                                            (if (= 0 point2-polarity point3-polarity point4-polarity point1-polarity) area 0))
                                                   (cond
                                                     (= 0 point2-polarity point3-polarity point4-polarity point1-polarity) area
                                                     :else 0)))



(defn d9p2 [filename] (let [points-s (clojure.string/split-lines (slurp filename))
                            points (map #(map read-string (clojure.string/split % #",")) points-s)
                            ]
                        (map #(let [[n1 n2] %
                                    point1 (nth points n1)
                                    point2 (nth points n2)] (valid-rectangle point1 point2 points)) (combo/combinations (range (count points)) 2)
                        )))

(d9p2 "input/day9_test.txt")








;; oLD WORKJ pART 2

(defn any-vertical-intersection [y1 y2 test-line] (let [[[test-x1 test-y1] [test-x2 test-y2]] test-line
                                                        min-test-y (min test-y1 test-y2)
                                                        max-test-y (max test-y1 test-y2)
                                                        miny (min y1 y2)
                                                        maxy (max y1 y2)
                                                        overlaps-min-in (< miny min-test-y maxy)
                                                        overlaps-max-in (< miny max-test-y maxy)
                                                        overlaps-covers (and (<= min-test-y miny) (>= max-test-y maxy))]
                                                    (or overlaps-min-in overlaps-max-in overlaps-covers)))


(defn ensure-no-vertical-intersection [candidate-point x1 y1 other-coords] (let [[candidate-x candidate-y] candidate-point
                                                                                 minx (min x1 candidate-x)
                                                                                 maxx (max x1 candidate-x)
                                                                                 possible-intersections (partition 2 (filter #(let [[ocx _] %] (and (> ocx minx) (< ocx maxx))) other-coords))]
                                                                             (not-any? #(any-vertical-intersection y1 candidate-y %) possible-intersections)))


(defn any-horizontal-intersections [x1 x2 test-line] (let [[[test-x1 test-y1] [test-x2 test-y2]] test-line
                                                           min-test-x (min test-x1 test-x2)
                                                           max-test-x (max test-x1 test-x2)
                                                           minx (min x1 x2)
                                                           maxx (max x1 x2)
                                                           overlaps-min-in (< minx min-test-x maxx)
                                                           overlaps-max-in (< minx max-test-x maxx)
                                                           overlaps-cover (and (<= min-test-x minx) (>= max-test-x maxx))
                                                           ]
                                                       (or overlaps-min-in overlaps-max-in overlaps-cover)))


(defn ensure-no-horizontal-intersection [candidate-point x1 y1 other-coords] (let [[candidate-x candidate-y] candidate-point
                                                                                   miny (min y1 candidate-y)
                                                                                   maxy (max y1 candidate-y)
                                                                                   possible-intersections (partition 2 (filter #(let [[_ ocy] %] (and (> ocy miny) (< ocy maxy))) other-coords))]
                                                                               (not-any? #(any-horizontal-intersections x1 candidate-x %) possible-intersections)))

(ensure-no-horizontal-intersection '(11 7) 7 1 '((2 3) (9 3) (2 5) (9 5) (1 4) (3 4)))
(ensure-no-horizontal-intersection '(11 7) 10 1 '((2 3) (9 3) (2 5) (9 5) (1 4) (3 4)))

(ensure-no-horizontal-intersection '(9 7) 2 3 '((2 5) (9 5)))
(ensure-no-horizontal-intersection '(9 7) 2 3 '((3 5) (8 5)))

(defn matches-sign-direction [test-point x y test-right test-down test-left test-up] (let [[test-x test-y] test-point
                                                                                           is-right (and (pos? (- test-x x)) (pos? (- test-y y)))
                                                                                           is-down (and (neg? (- test-x x)) (pos? (- test-y y)))
                                                                                           is-left (and (neg? (- test-x x)) (neg? (- test-y y)))
                                                                                           is-up (and (pos? (- test-x x)) (neg? (- test-y y)))]

                                                                                       (and
                                                                                        (= test-right is-right)
                                                                                        (= test-down is-down)
                                                                                        (= test-left is-left)
                                                                                        (= test-up is-up))))


(defn filter-points-by-direction [coord-num other-coords] (let [[x1 y1] (nth other-coords coord-num)
                                                                [dir-x dir-y] (nth (cycle other-coords) (inc coord-num))
                                                                remaining-coords (take (- (count other-coords) 2) (drop (+ 2 coord-num) (cycle other-coords)))
                                                                is-right (and (pos? (- dir-x x1)) (zero? (- dir-y y1)))
                                                                is-down (and (zero? (- dir-x x1)) (pos? (- dir-y y1)))
                                                                is-left (and (neg? (- dir-x x1)) (zero? (- dir-y y1)))
                                                                is-up (and (zero? (- dir-x x1)) (neg? (- dir-y y1)))
                                                                candidate-points (filter #(matches-sign-direction % x1 y1 is-right is-down is-left is-up) remaining-coords)
                                                                clears-horizontal (filter #(ensure-no-horizontal-intersection % x1 y1 other-coords) remaining-coords)
                                                                clears-vertical (filter #(ensure-no-vertical-intersection % x1 y1 other-coords) clears-horizontal)
                                                                sizes (map (fn [candidate-point] (let [[cx cy] candidate-point
                                                                                                       y (inc (- (max y1 cy) (min y1 cy)))
                                                                                                       x (inc (- (max x1 cx) (min x1 cx)))] (* x y))) clears-vertical)]
(println coord-num x1 y1 clears-horizontal clears-vertical sizes)

                                                            (if (> (count sizes) 0) (apply max sizes) 0)
                                                                   ))
(defn d9p2 [filename] (let [points (clojure.string/split-lines (slurp filename))
                            points-num (map #(map read-string (clojure.string/split % #",")) points)
                            ]
                        (apply max (map #(filter-points-by-direction % points-num) (range (count points-num))))
))                        

(d9p2 "input/day9_test.txt")
(d9p2 "input/day9.txt")
;; 1303232477 is too low 
