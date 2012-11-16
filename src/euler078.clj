;; EULER #078
;; ==========
;; Let p(n) represent the number of different ways in which n coins can be
;; separated into piles. For example, five coins can separated into piles
;; in exactly seven different ways, so p(5)=7.
;;
;;       OOOOO
;;     OOOO   O
;;     OOO   OO
;;    OOO   O   O
;;    OO   OO   O
;;   OO   O   O   O
;;  O   O   O   O   O
;;
;; Find the least value of n for which p(n) is divisible by one million.
;;

(defn pentagonal-num [n] 
  (quot (* n (dec (* 3 n))) 2))

(def sgn (cycle '(1 -1)))

(def integers (iterate inc 1))

(def p 
  (memoize
    (fn [n]
      (cond 
        (= n 0) 1
        (neg? n) 0
        :else (letfn [(term [k]
                        (+ (p (- n (pentagonal-num k)))
                           (p (- n (pentagonal-num (- k))))))]
                (mod 
                  (reduce +
                    (take-while (comp not zero?)
                      (map #(* (term %2) %1) sgn integers)))
                  1000000))))))

(defn solve []
  (first 
    (for [i integers
          :when (zero? (p i))]
      i)))

(time (solve))
