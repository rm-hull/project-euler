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
  (:use [util.misc]
        [clojure.string :only (split-lines split)]
        [clojure.set :only (difference union)]))

;; Construction

(defn create-cell [c]
  (let [i (- (int c) 48)]
    (into (sorted-set)
      (if (pos? i) 
        (list i)
        (range 1 10)))))

(defn create-grid 
  "Assumes the first line of the data is the grid name, and the remaining
   lines are the grid contents. The grid is constructed as an 81-element
   vector of sets, where each set contains the possible values for that
   cell. A set of size one implies that the cell contents is known and
   needs no further reduction."
  [data]
  [ (first data) 
    (->> (rest data)
         (apply str)
         (map create-cell)
         vec) ])

(defn load-data 
  "Loads the data from the given file, creating the grid and allocating
   into a map by the grid name."
  [fname] 
  (->> (slurp fname)
       split-lines
       (partition 10)
       (map create-grid)
       (into (sorted-map))))

;; Accessor functions

(defn offset [^long x ^long y]
  (+ x (* y 9)))

(defn norm [^long x]
  (* 3 (quot x 3)))

(defn cell 
  "Gets the cell contents in the grid at the given position"
  ([grid ^long x ^long y] (cell grid (offset x y))) 
  ([grid ^long offset] (nth grid offset)))

(defn slice [grid ^long offset ^long size]
  (subvec grid offset (+ offset size)))

;; TODO: Change to to use subvec
;(defn row 
;  "Gets the set of possible cell contents in the grid at the given row"
;  [grid ^long y]
;  (nth (partition 9 grid) y))

(defn row
  "Gets the set of possible cell contents in the grid at the given row"
  [grid ^long y]
  (slice grid (* y 9) 9))

(defn column 
  "Gets the set of possible cell contents in the grid at the given column"
  [grid ^long x]
  (take-nth 9 (drop x grid)))

(defn box 
  "Gets the set of possible cell contents in the grid at the given box, may
   be referred to by a box number 0..8, or (x,y) co-ords."
  ([grid ^long i] (box grid (* 3 (quot i 3)) (* 3 (rem i 3))))
  ([grid ^long x ^long y]
    (->> (range 3)
         (map #(offset (norm x) (+ % (norm y))))
         (map #(slice grid % 3))
         (apply concat))))


;; Strategies

(defn known-digits [& groups]
  (into (sorted-set) 
    (for [cells groups
          cell  cells
          :when (= (count cell) 1)]
      (first cell))))

(defn valid-digits 
  "Removes those possible values from the cell at (x,y) in the grid from 
   the known digits in the current row, column and square. If there is 
   nothing to remove, nil is returned, otherwise the set of possible 
   digits is returned."
  [grid ^long x ^long y]
  (not-empty 
    (difference 
      (cell grid x y)
      (known-digits
        (row grid y)
        (column grid x)
        (box grid x y)))))

(defn single-candidate-reduction 
  "Reduces the elements in the grid where any cells which have only one 
   candidate can safely be assigned that value."
  [grid]
  (vec 
    (for [y (range 9)
          x (range 9)
          :let [curr (cell grid x y)]]
      (if (= (count curr) 1)
        curr
        (if-let [poss (valid-digits grid x y)] poss curr)))))

(defn number-frequencies 
  "Count the number of times each digit appears across all 
   supplied collections."
  [coll] 
  (apply merge-with + (map frequencies coll))) 

(defn hidden-single 
  "Checks to see if any of the possible digits for cell at the given
   position is the only candidate, but is hidden among other candidates."
  [grid ^long x ^long y]
  (let [chk  (fn [f digit] (= 1 (get (number-frequencies f) digit)))
        uniq (for [poss (cell grid x y)
                   :when (or
                           (chk (row grid y) poss)
                           (chk (column grid x) poss)
                           (chk (box grid x y) poss))]
               poss)]
    (if (= (count uniq) 1) (set uniq))))

(defn hidden-single-reduction 
  "Reduces the elements in the grid where frequently, there is only one 
   candidate for a given row, column or box, but it is hidden among other 
   candidates."
  [grid]
  (vec 
    (for [y (range 9)
          x (range 9)
          :let [curr (cell grid x y)]]
      (if (= (count curr) 1)
        curr
        (if-let [poss (hidden-single grid x y)] poss curr)))))

(defn get-first-choice 
  "Returns a map comprised of the first cell which has a choice of 
   possible values, along with the offset in the grid."
  [grid]
  (->> (map #(hash-map :offset %1 :poss %2) (iterate inc 0) grid)
       (filter #(> (count (:poss %)) 1))
       first)) 

;; Termination functions

(defn complete-set? [coll]
  (let [digits (apply union coll)
        sum (reduce + digits)
        product (reduce * digits)]
    (and (= sum 45) (= product 362880))))

(defn solved? 
  "Checks to see if the grid is solved, that is, every cell has at most
   one possible digit assigned, and that each row, column and box
   contains the digits 1 through 9."
  [grid]
  (and 
    (every? #(= 1 (count %)) grid)
    (every? identity 
            (for [i (range 9)]
              (and 
                (complete-set? (row grid i))
                (complete-set? (column grid i))
                (complete-set? (box grid i)))))))

;; Solver functions
(def strategy-fn (comp single-candidate-reduction hidden-single-reduction))

(defn simple-solver 
  "Iteratively reduces down the named grid until either a solution is 
   found (and the solution is returned) or successive reductions produce
   no improvements."
  [grid]
  (loop [prev      nil 
         solutions (iterate strategy-fn grid)]
    (let [curr (first solutions)]
      (cond 
        (= prev curr)  { :solved false :grid curr }
        (solved? curr) { :solved true  :grid curr }
        :else          (recur curr (next solutions))))))
  
(defn what-if-solver 
  "Performs a depth-first search of all the possible grids, returned as a
   lazy sequence. It is up to the consumer of the sequence to terminate
   iterating over the sequence on receipt of a completed solution. Note
   that this is different to the simple solver implementation."
  [grid] 
  (let [choice (get-first-choice grid)
        next-gen (for [poss (:poss choice)]
                    (simple-solver
                      (assoc grid (:offset choice) (hash-set poss))))]
    (lazy-cat next-gen 
              (mapcat #(what-if-solver (:grid  %)) next-gen)))) 

(defn sudoku-solver 
  "Given a grid, applies a simple solver and returns the completed result
   if a solution is found, else goes on to do a brute force depth-first
   search over the pruned search space."
  [grid]
    (let [first-pass (simple-solver grid)]
      (if (:solved first-pass)
        first-pass
        (->> (what-if-solver (:grid first-pass))
             (filter :solved)
             first))))

(defn top-left-digits 
  "Strips the top-leftmost digits from the Sudoku grid, and converts to
   a number"
  [solution]
  (->> (:grid solution)
       (take 3)
       (map first)
       to-number)) 

(defn solve [fname]
  (->> (vals (load-data fname))
       (pmap (comp top-left-digits sudoku-solver))
       (reduce +)))

(time (solve "data/sudoku.txt"))

(def hardest-sudokus (vals (load-data "data/worlds-hardest-sudoku.txt")))

(time 
  (doseq [x (map sudoku-solver hardest-sudokus)]
    (prn x)))




