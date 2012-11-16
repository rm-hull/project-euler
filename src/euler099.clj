;; EULER #099
;; ==========
;; Comparing two numbers written in index form like 2^11 and 3^7 is not
;; difficult, as any calculator would confirm that 2^11 = 2048 < 3^7 = 2187.
;;
;; However, confirming that 632382^518061 > 519432^525806 would be much more
;; difficult, as both numbers contain over three million digits.
;;
;; Using data/base_exp.txt, a 22K text file containing one thousand lines
;; with a base/exponent pair on each line, determine which line number has
;; the greatest numerical value.
;;
;; NOTE: The first two lines in the file represent the numbers in the example
;; given above.
;;
 
(use 'clojure.string)
 
(defn monatomic-exp-fn [[base exp]]
  (* exp (Math/log base)))
 
(defn read-numbers [s]
  (map read-string (split s #",")))
 
(defn get-data [fname]
  (map read-numbers
       (split-lines (slurp fname))))
 
(defn solve [fname]
  (letfn [(maxf [a b] (if (> (:fn a) (:fn b)) a b))]
    (reduce maxf
            (map #(hash-map :fn (monatomic-exp-fn %1) :input %1 :count %2)
                 (get-data fname)
                 (iterate inc 1)))))
 
(time (solve "data/base_exp.txt"))
