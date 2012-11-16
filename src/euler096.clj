;; EULER #096
;; ==========
;; Su Doku (Japanese meaning number place) is the name given to a popular 
;; puzzle concept. Its origin is unclear, but credit must be attributed to 
;; Leonhard Euler who invented a similar, and much more difficult, puzzle 
;; idea called Latin Squares. The objective of Su Doku puzzles, however, 
;; is to replace the blanks (or zeros) in a 9 by 9 grid in such that each 
;; row, column, and 3 by 3 box contains each of the digits 1 to 9. Below 
;; is an example of a typical starting puzzle grid and its solution grid.
;;
;;     0 0 3   0 2 0   6 0 0         4 8 3   9 2 1   6 5 7  
;;     9 0 0   3 0 5   0 0 1         9 6 7   3 4 5   8 2 1  
;;     0 0 1   8 0 6   4 0 0         2 5 1   8 7 6   4 9 3
;;
;;     0 0 8   1 0 2   9 0 0         5 4 8   1 3 2   9 7 6
;;     7 0 0   0 0 0   0 0 8   ==>   7 2 9   5 6 4   1 3 8
;;     0 0 6   7 0 8   2 0 0         1 3 6   7 9 8   2 4 5
;; 
;;     0 0 2   6 0 9   5 0 0         3 7 2   6 9 8   5 1 4
;;     8 0 0   2 0 3   0 0 9         8 1 4   2 5 3   7 6 9
;;     0 0 5   0 1 0   3 0 0         6 9 5   4 1 7   3 8 2
;;
;; A well constructed Su Doku puzzle has a unique solution and can be 
;; solved by logic, although it may be necessary to employ "guess and test" 
;; methods in order to eliminate options (there is much contested opinion 
;; over this). The complexity of the search determines the difficulty of 
;; the puzzle; the example above is considered easy because it can be 
;; solved by straight forward direct deduction.
;;
;; The 6K text file, 'data/sudoku.txt', contains fifty different Su Doku 
;; puzzles ranging in difficulty, but all with unique solutions (the first 
;; puzzle in the file is the example above).
;;
;; By solving all fifty puzzles find the sum of the 3-digit numbers found 
;; in the top left corner of each solution grid; for example, 483 is the 
;; 3-digit number found in the top left corner of the solution grid above.
;;

(ns euler096
  (:use [clojure.string :only (split-lines split)]
        [clojure.set :only (difference)]))

(defn create-cell [c]
  (let [i (- (int c) 48)]
    (into (sorted-set)
      (if (pos? i) 
        (list i)
        (range 1 10)))))

(defn create-grid [data]
  [ (first data) 
    (->> (rest data)
         (apply str)
         (map create-cell)
         vec) ])

(defn load-data [fname] 
  (->> (slurp fname)
       split-lines
       (partition 10)
       (map create-grid)
       (into (sorted-map))))

(defn cell-pos [x y]
  (+ x (* y 9)))
  
(defn cell [grid x y]
  (nth grid (cell-pos x y)))

(defn row [grid y]
  (nth (partition 9 grid) y))

(defn column [grid x]
  (take-nth 9 (drop x grid)))

(defn square [grid x y]
  (->> (nth (partition 27 grid) (quot y 3))
       (partition 3)
       (drop (quot x 3))
       (take-nth 3)
       (apply concat)))

(defn known-digits [& groups]
  (into (sorted-set) 
    (for [cells groups
          cell  cells
          :when (= (count cell) 1)]
      (first cell))))

(defn valid-digits [grid x y]
  (difference 
    (cell grid x y)
    (known-digits
      (row grid y)
      (column grid x)
      (square grid x y))))
  
(defn solved? [grid]
  (every? #(= 1 (count %)) grid))

(defn solve [f grid]
  (loop [i         0
         prev      nil 
         solutions (iterate f grid)]
    (let [curr (first solutions)]
      (cond 
        (solved? curr) { :solved true  :iterations i :grid curr }
        (= prev curr)  { :solved false :iterations i :grid curr }
        :else          (recur (inc i) curr (next solutions))))))
  
(defn single-candidate-reduction [grid]
  (for [y (range 9)
        x (range 9)
        :let [cell (cell grid x y)
              poss (valid-digits grid x y)]]
   (if (empty? poss) cell poss)))

; http://angusj.com/sudoku/hints.php

(def grid1 (get (load-data "data/sudoku.txt")  "Grid 01"))
(def grid2 (get (load-data "data/sudoku.txt")  "Grid 02"))
(def grid3 (get (load-data "data/sudoku.txt")  "Grid 03"))

(solve single-candidate-reduction grid1)


