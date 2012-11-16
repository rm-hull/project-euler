;; EULER #031
;; ==========
;; In England the currency is made up of pound, £, and pence, p, and 
;; there are eight coins in general circulation:
;;
;;    1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
;;
;; It is possible to make Â£2 in the following way:
;; 
;;    1x£1 + 1x50p + 2x20p + 1x5p + 1x2p + 3x1p
;;
;; How many different ways can £2 be made using any number of coins?
;;

(ns euler031)

(def coins [200 100 50 20 10 5 2 1])

(defn change[c v]
  (if (= (count c) 1) 
    1
    (let [f (first c)]
      (reduce + 
        (for [n (range 0 (inc (quot v f)))]
          (change (rest c) (- v (* n f))))))))

(defn solve []
  (change coins 200))

(time (solve))

