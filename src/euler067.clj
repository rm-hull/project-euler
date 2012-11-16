;; EULER #067
;; ==========
;; By starting at the top of the triangle below and moving to adjacent 
;; numbers on the row below, the maximum total from top to bottom is 23.
;;
;;       3
;;      7 4
;;     2 4 6
;;    8 5 9 3
;;
;; That is, 3 + 7 + 4 + 9 = 23.
;; 
;; Find the maximum total from top to bottom in 'data/100-triangle.txt, a
;; 15K text file containing a triangle with one-hundred rows.
;;
;; NOTE: This is a much more difficult version of Problem 18. It is not 
;; possible to try every route to solve this problem, as there are 2^99 
;; altogether! If you could check one trillion (10^12) routes every 
;; second it would take over twenty billion years to check them all. 
;; There is an efficient algorithm to solve it. ;o)
;;

(ns euler067)

(defn get-triangle [fname]
  (letfn [(parse-nums [s] (map #(Integer/parseInt %) (clojure.string/split s #"\W")))]
    (map parse-nums (clojure.string/split-lines (slurp fname)))))

(defn max-pairs [xs]
  (map (partial apply max) (partition-all 2 1 xs)))

(defn calc [triangle]
  (loop [data (rest triangle)
         accum (first triangle)]
    (if (empty? data)
      accum   
      (recur 
        (rest data)
        (map + (max-pairs accum) (first data))))))   

(defn solve [fname]
  (calc (reverse (get-triangle fname))))

(time (solve "data/100-triangle.txt"))
