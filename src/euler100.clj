;; EULER #100
;; ==========
;; If a box contains twenty-one coloured discs, composed of fifteen blue
;; discs and six red discs, and two discs were taken at random, it can be
;; seen that the probability of taking two blue discs, 
;;
;;    P(BB) = (15/21)x(14/20) = 1/2.
;;
;; The next such arrangement, for which there is exactly 50% chance of 
;; taking two blue discs at random, is a box containing eighty-five blue 
;; discs and thirty-five red discs.
;;
;; By finding the first arrangement to contain over 10^12 
;; [ = 1,000,000,000,000 ] discs in total, determine the number of blue 
;; discs that the box would contain.
;;

(ns euler100)

(def pairs-seq
  (letfn [(next-pair [b n] 
            (lazy-seq 
              (cons {:blue b, :total n} 
                    (next-pair 
                      (+ (* 3 b) (* 2 n) -2) 
                      (+ (* 4 b) (* 3 n) -3)))))]
    (next-pair 1 1)))

(defn solve [n]
  (first 
    (drop-while #(< (:total %) n) pairs-seq)))

(time (solve 1e12))
