(ns aoc2025.day4
  (:require
   [clojure.string :as string]))

(slurp "input/day4_test.txt")
;; Width of columns 
(clojure.string/index-of (slurp "input/day4_test.txt") "\n")
;; Number of columns 
(count (clojure.string/replace (slurp "input/day4_test.txt") #"[^\n]" ""))


;; Bitmap 
(clojure.string/replace (slurp "input/day4_test.txt") "\n" "")


(defn has-idx [nrows ncols current-loc row-offset col-offset] (let [check-left (< col-offset 0)
                                                                    check-right (> col-offset 0)
                                                                    check-up (< row-offset 0)
                                                                    check-down (> row-offset 0)
                                                                    in-first-col (= (mod current-loc ncols) 0)
                                                                    in-last-col (= (mod current-loc ncols) (- ncols 1))
                                                                    in-first-row (= (quot current-loc nrows) 0)
                                                                    in-last-row (= (quot current-loc nrows) (- nrows 1))]
                                                                (cond
                                                                  (>= current-loc (* nrows ncols)) 0
                                                                  ;; Lefts
                                                                  (and check-left in-first-col) 0
                                                                  (and check-left in-first-col check-up in-first-row) 0
                                                                  (and check-left in-first-col check-down in-last-row) 0
                                                                  ;; Rights 
                                                                  (and check-right in-last-col) 0
                                                                  (and check-right in-last-col check-up in-first-row) 0
                                                                  (and check-right in-last-col check-down in-last-row) 0
                                                                  ;; Ups 
                                                                  (and check-up in-first-row) 0
                                                                  ;; Downs 
                                                                  (and check-down in-last-row) 0
                                                                  :else 1)))
(has-idx 4 4 2 -1 0)
(has-idx 4 4 3 0 1)
(has-idx 4 4 4 0 1)
(has-idx 4 4 3 1 1)
(has-idx 4 4 3 -1 1)
(has-idx 4 4 15 -1 0)



(defn paper-at-offset [bitmap nrows ncols current-loc row-offset col-offset] (if (= 1 (has-idx nrows ncols current-loc row-offset col-offset))
                                                                               (let [bitmap-low (+ current-loc (* row-offset ncols) col-offset)
                                                                                     bitmap-high (+ bitmap-low 1)] (cond
                                                                                                                     (< (count bitmap) bitmap-low) 0
                                                                                                                     (= "@" (subs bitmap bitmap-low bitmap-high)) 1
                                                                                                                     :else 0))
                                                                               0))

(paper-at-offset "..@...@...@...@." 4 4 6 0 0)
(paper-at-offset "..@...@...@...@." 4 4 6 -1 0)
(paper-at-offset "..@...@...@...@." 4 4 6 1 1)

(defn count-adjacent [bitmap nrows ncols current-location] (if (= 1 (paper-at-offset bitmap nrows ncols current-location 0 0))
                                                             (- (reduce + (for [row-offset [-1 0 1]
                                                                                col-offset [-1 0 1]]
                                                                            (paper-at-offset bitmap nrows ncols current-location row-offset col-offset))) 1)
                                                             99))

(reduce + (for [row-offset [-1 0 1]
                col-offset [-1 0 1]]
            (paper-at-offset "..@...@...@...@." 4 4 6 row-offset col-offset)))

(count-adjacent "..@...@...@...@." 4 4 6)

(paper-at-offset "..@...@...@...@." 4 4 6 0 0)



(defn d4p1 [filename fewer-threshold]
  (let [raw-bitmap (slurp filename)
        bitmap (clojure.string/replace raw-bitmap "\n" "")
        ncols (clojure.string/index-of raw-bitmap "\n")
        nrows (count (clojure.string/replace raw-bitmap #"[^\n]" ""))]


    (reduce + (map (fn [x] (if
                            (< (count-adjacent bitmap nrows ncols x) fewer-threshold)
                             1
                             0)) (range 0 (* ncols nrows))))))




(d4p1 "input/day4_test.txt" 4)
(d4p1 "input/day4.txt" 4)


;; Part 2

(defn remove-extractable-paper [bitmap nrows ncols fewer-threshold] (clojure.string/join (map (fn [x] (cond
                                                                                                        (> (count-adjacent bitmap nrows ncols x) 10) "."
                                                                                                        (< (count-adjacent bitmap nrows ncols x) fewer-threshold) "."
                                                                                                        (= (paper-at-offset bitmap nrows ncols x 0 0) 1) "@"
                                                                                                        :else "."))
                                                                                              (range 0 (* nrows ncols)))))




(defn count-rolls-in-bitmap [bitmap] (count (filter (fn [x] (= x "@")) (clojure.string/split bitmap #""))))
(count-rolls-in-bitmap "..@..@@..@@..")

(defn d4p2 [filename fewer-threshold]
  (let [raw-bitmap (slurp filename)
        bitmap (clojure.string/replace raw-bitmap "\n" "")
        ncols (clojure.string/index-of raw-bitmap "\n")
        nrows (count (clojure.string/replace raw-bitmap #"[^\n]" ""))
        previous-bitmap nil]

    (loop [bmap bitmap
           bmap-previous previous-bitmap]
      (when (not= bmap bmap-previous)
        (println (str (count-rolls-in-bitmap bmap) "->" (- (count-rolls-in-bitmap bitmap) (count-rolls-in-bitmap bmap))))
        (recur (remove-extractable-paper bmap ncols nrows fewer-threshold) bmap)))))


(d4p2 "input/day4_test.txt" 4)
(d4p2 "input/day4.txt" 4)

