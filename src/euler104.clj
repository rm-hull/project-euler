;; EULER #104
;; ==========
;; The Fibonacci sequence is defined by the recurrence relation:
;;
;;    F[n] = F[n-1] + F[n-2], where F[1] = 1 and F[2] = 1.
;;
;; It turns out that F[541], which contains 113 digits, is the first Fibonacci
;; number for which the last nine digits are 1-9 pandigital (contain all the 
;; digits 1 to 9, but not necessarily in order). And F[2749], which contains 
;; 575 digits, is the first Fibonacci number for which the first nine digits 
;; are 1-9 pandigital.
;;
;; Given that F[k] is the first Fibonacci number for which the first nine
;; digits AND the last nine digits are 1-9 pandigital, find k.
;;

(ns euler104)

(defn tail-limiting-fib-seq [modulus]
  (letfn [(limit [n] (if (> n modulus) (rem n modulus) n))
          (seq0 [a b] (lazy-seq (cons a (seq0 b (limit (+ a b))))))]
    (seq0 1 1)))

(defn pandigital? [^long n]
  (if (< n 123456789)
    false
    (loop [n      n
           digits 0]
      (if (zero? n)
        (= digits 1022)
        (recur 
          (quot n 10)
          (bit-or 
            digits
            (bit-shift-left 1 (- n (* (quot n 10) 10)))))))))

; Binet's formula for working out n'th Fibonacci number
;   F[n] = phi^n/sqrt(5)
;
; log10(F[n]) = log10(phi^n/sqrt(5))
; log10(F[n]) = n * log10(phi) - log10(sqrt(5))

(def log10-sqrt5 (Math/log10 (Math/sqrt 5))) 

(def log10-phi (Math/log10 golden-ratio))

(defn head-limiting-fib [^long n] 
  (let [t (- (* n log10-phi) log10-sqrt5)]
    (long (Math/pow 10 (- t (long t) -8))))) ; limit to most significant 9 digits

(defn solve []
  (->> 
    (map vector integers (tail-limiting-fib-seq 1000000000))
    (filter #(pandigital? (second %)))
    (filter #(pandigital? (head-limiting-fib (first %)))) 
    first))

(time (solve))
