;; EULER #083
;; ==========
;; NOTE: This problem is a significantly more challenging version of
;; Problem 81.
;;
;; In the 5 by 5 matrix below, the minimal path sum from the top left 
;; to the bottom right, by moving left, right, up, and down, is indicated
;; with brackets and is equal to 2297.
;; 
;;   [131] 673 [234][103][ 18]
;;   [201][ 96][342] 965 [150]
;;    630  803  746 [422][111]
;;    537  699  497 [121] 956
;;    805  732  524 [ 37][331]
;;
;; Find the minimal path sum, in 'data/matrix.txt', a 31K text file
;; containing a 80 by 80 matrix, from the top left to the bottom right by
;; moving left, right, up, and down.
;; 

(ns euler083
  (:use [util.dijkstra]))

(def neighbours
  "Allows up/down/forwards/backwards"
  (memoize
    (fn [^long p [^long w ^long h]]
      (->> [(- p w) (+ p w)]
           (add-if (> (rem p w) 0) (dec p))
           (add-if (< (rem p w) (dec w)) (inc p))
           (filter #(and (>= % 0) (< % (* w h))))))))

(defn solve [fname]
  (let [matrix (get-data fname)
        from   0
        to     (dec (apply * (:size matrix)))]
    (->> (shortest-path matrix neighbours from to)
         (map (partial weight matrix))
         (reduce +))))

(time (solve "data/5-matrix.txt"))

(time (solve "data/80-matrix.txt"))

