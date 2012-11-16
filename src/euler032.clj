;; EULER #032
;; ==========
;; We shall say that an n-digit number is pandigital if it makes use of all
;; the digits 1 to n exactly once; for example, the 5-digit number, 15234, 
;; is 1 through 5 pandigital.
;;
;; The product 7254 is unusual, as the identity, 39 x 186 = 7254, 
;; containing multiplicand, multiplier, and product is 1 through 9 
;; pandigital.
;;
;; Find the sum of all products whose multiplicand/multiplier/product 
;; identity can be written as a 1 through 9 pandigital.
;;
;; HINT: Some products can be obtained in more than one way so be sure to 
;; only include it once in your sum.
;;

(ns euler032)

(defn is-pandigital? [s]
  (= "123456789" (apply str (sort s))))

(defn solve [] 
  (reduce + 
    (distinct 
      (for [a (range 2 5000)
            b (range a (/ 9999 a))
            :when (is-pandigital? (str a b (* a b)))]
        (* a b)))))

(time (solve))
