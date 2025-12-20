(ns aoc2025.day7
  (:require
   [clojure.string :as string]))


;; Get row below in bitmap 
(defn row-below [bitmap nrows ncols shoot-origin] (cond
                                                    (>= shoot-origin (* nrows ncols)) "^"
                                                    (>= (+ shoot-origin ncols) (* nrows ncols)) "^"
                                                    :else (subs bitmap (+ shoot-origin ncols) (+ shoot-origin ncols 1))))
(defn bitmap-after-shoot [bitmap nrows ncols shoot-origin]
  (if (>= (+ shoot-origin ncols) (* ncols nrows)) bitmap
      (str
       (subs bitmap 0 (+ shoot-origin ncols))
       "|"
       (subs bitmap (+ shoot-origin ncols 1)))))

(defn bitmap-after-split [bitmap split-origin] (str
                                                (subs bitmap 0 (dec split-origin))
                                                "|X|"
                                                (subs bitmap (+ 2 split-origin))))


(bitmap-after-shoot ".S......." 3 3 1)

(defn bitmap-after-nobeam [bitmap split-origin] (str
                                                 (subs bitmap 0 split-origin)
                                                 "O"
                                                 (subs bitmap (inc split-origin))))

;; Shoot bitmap down from starting location
(defn shoot-tachyon [bitmap nrows ncols shoot-origin] (loop [current-shoot shoot-origin
                                                             bmap bitmap]
                                                        (cond
                                                          (>= (+ current-shoot ncols) (* nrows ncols)) bmap
                                                          (not= (row-below bmap nrows ncols current-shoot) "^") (let [new-bmap (bitmap-after-shoot bmap nrows ncols current-shoot)
                                                                                                                      next-shoot (+ current-shoot ncols)]
                                                                                                                  (recur next-shoot new-bmap))
                                                          :else bmap)))

(defn split-tachyon [bitmap nrows ncols split-origin num-splits] (let [not-split (not= (subs bitmap split-origin (inc split-origin)) "^")
                                                                       not-beamed (not= (subs bitmap (- split-origin ncols) (- split-origin ncols -1)) "|")
                                                                       one-before (not= (subs bitmap (dec split-origin) split-origin) "|")
                                                                       one-after (not= (subs bitmap (inc split-origin) (+ split-origin 2)) "|")
                                                                       handled-bitmap (bitmap-after-split bitmap split-origin)]

                                                                   (cond
                                                                     not-split bitmap
                                                                     not-beamed (bitmap-after-nobeam bitmap split-origin)
                                                                     :else (shoot-tachyon (shoot-tachyon handled-bitmap nrows ncols (dec split-origin)) nrows ncols (inc split-origin)))))

(defn perform-beam-splitting [filename] (let [raw-bitmap (slurp filename)
                                              bitmap (clojure.string/replace raw-bitmap "\n" "")
                                              num-splits (count (clojure.string/replace bitmap #"[^\^]" ""))
                                              ncols (clojure.string/index-of raw-bitmap "\n")
                                              nrows (count (clojure.string/replace raw-bitmap #"[^\n]" ""))
                                              start_idx (clojure.string/index-of bitmap "S")
                                              first-shootmap (shoot-tachyon bitmap nrows ncols start_idx)]

                                          (loop [current-bmap first-shootmap]
                                            (if (not (nil? (clojure.string/index-of current-bmap "^")))
                                              (let [split-location (clojure.string/index-of current-bmap "^")
                                                    new-bmap (split-tachyon current-bmap nrows ncols split-location 0)]
                                                (recur new-bmap))
                                              current-bmap))))

;; Iterate through splits -> see if beam above, do not double count
(defn d7p1 [filename] (let [raw-bitmap (slurp filename)
                            bitmap (clojure.string/replace raw-bitmap "\n" "")
                            num-splits (count (clojure.string/replace bitmap #"[^\^]" ""))
                            split-beams (perform-beam-splitting filename)
                            remaining-splitters (count (clojure.string/replace split-beams #"[^O]" ""))]

                        (- num-splits remaining-splitters)))


(d7p1 "input/day7_test.txt")
(d7p1 "input/day7.txt")
;; 1720 is too high  
;; 1619 is answer!

;; Rewrite to handle row by row

;; Handle first row with S 
;; For remaining rows, have list of values
(defn count-splits [current-density split-str split-loc] (let [at-split (= (subs split-str split-loc (inc split-loc)) "^")
                                                               has-right (< split-loc (dec (count split-str)))
                                                               has-left (> split-loc 0)
                                                               left-split (if has-left (= (subs split-str (dec split-loc) split-loc) "^") false)
                                                               right-split (if has-right (= (subs split-str (inc split-loc) (+ split-loc 2)) "^") false)
                                                               density-left (if (and has-left left-split) (get current-density (dec split-loc)) 0)
                                                               density-middle (if at-split 0 (get current-density split-loc))
                                                               density-right (if (and has-right right-split) (get current-density (inc split-loc)) 0)] 
                                                                                                                                     ( cond
                                                                                                                                       at-split 0
                                                                                                                                       (and has-left has-right) (+ density-left density-middle density-right)
                                                                                                                                       has-left (+ density-left density-middle)
                                                                                                                                       has-right (+ density-middle density-right))
                                                           ))


(count-splits [0 2 0] ".^." 2)
(count-splits [0 1 0 2 0] ".^.^." 2)
(count-splits [0 1 0 2 0] ".^.^." 0)
(count-splits [0 1 0 2 0] ".^.^." 4)
(count-splits [0 1 0 0 0] ".S..." 5)

(map #(count-splits [0 1 0 2 0 3 0] ".^.^..." %) (range 7))

(defn apply-splits [current-density split-str] (mapv #(count-splits current-density split-str %) (range (count split-str))))

(apply-splits [0 1 0 2 0 3 0] ".^.....")

;; (reduce apply-splits [init-list] [split-lines])
(defn first-line-to-vector [s] (
                              mapv (fn [x] (if (= (subs s x (inc x)) "S") 1 0)) (range (count s))) )

(first-line-to-vector "..S....")

(defn d7p2 [filename] (let [
                            raw-bitmap (slurp filename)
                            bitmap-lines (clojure.string/split-lines raw-bitmap)
                            first-line (first-line-to-vector (first bitmap-lines))
                            ]
                        (reduce + (reduce apply-splits first-line bitmap-lines))
                        
                        ))


(d7p2 "input/day7_test2.txt")
(d7p2 "input/day7_test.txt")
(d7p2 "input/day7.txt")