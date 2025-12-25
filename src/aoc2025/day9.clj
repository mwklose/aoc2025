(ns aoc2025.day9
  (:require
   [clojure.string :as string]
   [clojure.math :as math]
   [clojure.math.combinatorics :as combo]))


(defn combo-to-area [corners] (let [[c1 c2] corners
                                    [x1 y1] (map read-string (clojure.string/split c1 #","))
                                    [x2 y2] (map read-string (clojure.string/split c2 #","))]
                                (* (inc (abs (- x2 x1))) (inc (abs (- y2 y1))))))


(defn d9p1 [filename] (let [points (clojure.string/split-lines (slurp filename))
                            combos (combo/combinations points 2)]
                        (apply max (map combo-to-area combos))))

(d9p1 "input/day9_test.txt")
(d9p1 "input/day9.txt")


;; Part 2

;; TODO: handle current error
(defn reduce-valid-rectangles [rowmap colmap ncols bitmap max-area point-pairs] (let [[point1 point2] point-pairs
                                                                                      [x1 y1] point1
                                                                                      [x2 y2] point2
                                                                                      [minx maxx] (sort (list x1 x2))
                                                                                      [miny maxy] (sort (list y1 y2))
                                                                                      map-minx (get rowmap minx)
                                                                                      map-maxx (get rowmap maxx)
                                                                                      map-miny (get colmap miny)
                                                                                      map-maxy (get colmap maxy)
                                                                                      idxs (for [x (range map-minx (inc map-maxx))
                                                                                                 y (range map-miny (inc map-maxy))] (+ x (* y ncols)))

                                                                                      valid-tiles (map #(not= "." (subs bitmap % (inc %))) idxs)
                                                                                      all-valid-tiles (reduce #(and %1 %2) true valid-tiles)
                                                                                      new-area (* (inc (- maxy miny)) (inc (- maxx minx)))
                                                                                      ]
                                                                                  
                                                                                  (cond 
                                                                                    (<= new-area max-area) max-area
                                                                                    all-valid-tiles (do (println max-area point-pairs new-area bitmap valid-tiles) new-area)
                                                                                    :else max-area
                                                                                    )
                                                                                  ))


(defn fill-bitmap-horizontally [bitmap nrows ncols] (let [row-chunks (map #(subs bitmap (* ncols %) (* ncols (inc %))) (range nrows))
                                                          row-replace (map (fn [x]
                                                                             (clojure.string/replace x
                                                                                                     #"([RG])(\.+)([RG])"
                                                                                                     #(str (get % 1) (apply str (repeat (count (get % 2)) "G")) (get % 3))
                                                                                                     ;;  #"([RG])(\.+)([RG])" 
                                                                                                     ;;  #(str %1 %3)
                                                                                                     ))
                                                                           row-chunks)]
                                                      (clojure.string/join row-replace)))

(fill-bitmap-horizontally "...R.R..." 3 3)
(fill-bitmap-horizontally "....R..R...." 3 4)
(fill-bitmap-horizontally "....R..G...." 3 4)

(defn update-bitmap-rows [ncols bitmap xmin xmax y] (clojure.string/join (map (fn [i] (cond
                                                                                        (and (= (quot i ncols) y) (= xmin (mod i ncols))) "R"
                                                                                        (and (= (quot i ncols) y) (= xmax (mod i ncols))) "R"
                                                                                        (and (= (quot i ncols) y) (<= xmin (mod i ncols) xmax)) "G"
                                                                                        :else (subs bitmap i (inc i)))) (range (count bitmap)))))

(update-bitmap-rows 3 "........." 0 1 1)




(defn update-bitmap-cols [ncols bitmap x ymin ymax] (clojure.string/join (map (fn [i] (cond
                                                                                        (= i (+ x (* ymin ncols))) "R"
                                                                                        (= i (+ x (* ymax ncols))) "R"
                                                                                        (and (= x (mod i ncols)) (<= ymin (quot i ncols) ymax)) "G"
                                                                                        :else (subs bitmap i (inc i)))) (range (count bitmap)))))

(update-bitmap-cols 3 "........." 1 0 2)

(defn reduce-bitmap-points [nrows ncols rowmap colmap bitmap point-combos] (let
                                                                            [[point1 point2] point-combos
                                                                             [x1 y1] point1
                                                                             [x2 y2] point2

                                                                             map-x1 (get rowmap x1)
                                                                             map-x2 (get rowmap x2)

                                                                             map-y1 (get colmap y1)
                                                                             map-y2 (get colmap y2)]

                                                                             (cond
                                                                               (= x1 x2) (update-bitmap-cols ncols bitmap map-x1 (min map-y1 map-y2) (max map-y1 map-y2))
                                                                               (= y1 y2) (update-bitmap-rows ncols bitmap (min map-x1 map-x2) (max map-x1 map-x2) map-y1)
                                                                               :else bitmap)))




(defn get-unique-entries [point-pairs] (let [x-points (map #(let [[e1 e2] %] e1) point-pairs)
                                             y-points (map #(let [[e1 e2] %] e2) point-pairs)
                                             sorted-x (sort (distinct x-points))
                                             sorted-y (sort (distinct y-points))
                                             len-x (count sorted-x)
                                             len-y (count sorted-y)]
                                         (list
                                          (zipmap sorted-x (range len-x))
                                          (zipmap sorted-y (range len-y))
                                          len-x
                                          len-y)))

(defn d9p2 [filename] (let
                       [points (clojure.string/split-lines (slurp filename))
                        point-pairs (map (fn [x] (map read-string (clojure.string/split x #","))) points)
                        point-partitions (partition 2 1 (take (inc (count point-pairs)) (cycle point-pairs)))
                        [unique-x unique-y len-x len-y] (get-unique-entries point-pairs)
                        partial-bitmap (partial reduce-bitmap-points len-x len-y unique-x unique-y)
                        bitmap (reduce partial-bitmap (apply str (repeat (* len-x len-y) ".")) point-partitions)
                        filled-bitmap (fill-bitmap-horizontally bitmap len-x len-y)
                        point-combinations (combo/combinations point-pairs 2)
                        partial-reduce-bitmap (partial reduce-valid-rectangles unique-x unique-y len-y filled-bitmap)]

                        (reduce partial-reduce-bitmap 0 point-combinations)))

(d9p2 "input/day9_test.txt")
(d9p2 "input/day9.txt")


;; (d9p2 "input/day9_test.txt")
;; (d9p2 "input/day9.txt")
;; 1303232477 is too low 
;; 2425347246 is too high
;; 1450414119
