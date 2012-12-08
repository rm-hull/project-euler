;; EULER #082
;; ==========
;; NOTE: This problem is a more challenging version of Problem 81.
;;
;; The minimal path sum in the 5 by 5 matrix below, by starting in any cell
;; in the left column and finishing in any cell in the right column, and 
;; only moving up, down, and right, is indicated with brackets; the sum is
;; equal to 994.
;;
;;    131  673 [234][103][ 18]
;;   [201][ 96][342] 965  150 
;;    630  803  746  422  111 
;;    537  699  497  121  956
;;    805  732  524   37  331 
;
;; Find the minimal path sum, in '80-matrix.txt', a 31K text file containing
;; a 80 by 80 matrix, from the left column to the right column.
;;

(ns euler082
  (:use [util.dijkstra]))

(def neighbours
  "Allows up/down/forwards (but not backwards)"
  (memoize
    (fn [^long p [^long w ^long h]]
      (->> [(- p w) (+ p w)]
           ;(add-if (> (rem p w) 0) (dec p))
           (add-if (< (rem p w) (dec w)) (inc p))
           (filter #(and (>= % 0) (< % (* w h))))))))

(defn min-path-length [matrix from]
  (let [[w h] (:size matrix)
        pred  (build-predecessors matrix neighbours from (dec (* w h)))]
    (reduce min
      (for [to (range (dec w) (* w h) w)]
        (->> (get-path to pred)
             (map (partial weight matrix))
             (reduce +))))))

(defn solve [fname]
  (let [matrix (get-data fname)
        [w h] (:size matrix)]
    (->> (range 0 (* w h) w)
         (pmap (partial min-path-length matrix))
         (reduce min)))) 

(time (solve "data/80-matrix.txt"))
