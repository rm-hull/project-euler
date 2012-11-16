;; EULER #095
;; ==========
;; The proper divisors of a number are all the divisors excluding the number
;; itself. For example, the proper divisors of 28 are 1, 2, 4, 7, and 14. As
;; the sum of these divisors is equal to 28, we call it a perfect number.
;;
;; Interestingly the sum of the proper divisors of 220 is 284 and the sum 
;; of the proper divisors of 284 is 220, forming a chain of two numbers. 
;; For this reason, 220 and 284 are called an amicable pair.
;;
;; Perhaps less well known are longer chains. For example, starting with 
;; 12496, we form a chain of five numbers:
;;
;;    12496 --> 14288 --> 15472 --> 14536 --> 14264  --> ( 12496  ...)
;;
;; Since this chain returns to its starting point, it is called an amicable
;; chain.
;;
;; Find the smallest member of the longest amicable chain with no element 
;; exceeding one million.
;;

(ns euler095
  (:use [util.misc]))

(defn amicable-chain 
  "Aliquot sequence starting with n, terminates the chain if an element was
   previously seen. Any of the Lehmer five [276, 552, 564, 660, and 966] are
   likely to lead to integer overflow."
  [^long n]
  (letfn [(seq0 [^long n seen]
            (if (seen n)
              (list n)
              (lazy-seq (cons n (seq0 (aliquot-sum n) (conj seen n))))))]
    (seq0 n #{0}))) ; seen is seeded with zero to ensure termination
          
(defn exceeds-a-million? [^long n]
  (> n 1000000))

(defn max-length [xs ys]
  (if (> (count xs) (count ys)) xs ys))

(defn solve []
  (->> (range 1000000)
       (map amicable-chain)
       (remove #(some exceeds-a-million? %))
       (filter #(= (first %) (last %)))
       (reduce max-length)
       (reduce min)))

(time (solve))
