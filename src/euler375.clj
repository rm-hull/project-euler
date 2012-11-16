;; EULER #375
;; ==========
;; Let S[n] be an integer sequence produced with the following pseudo-random
;; number generator:
;;
;;    S[0]   = 290797 
;;    S[n+1] = S[n]^2 mod 50515093
;;
;; Let A(i, j) be the minimum of the numbers S[i], S[i+1], ... , S[j] 
;; for i <= j.
;;
;; Let M(N) = ΣA(i, j) for 1 <= i <= j <= N.
;; 
;; We can verify that M(10) = 432256955 and M(10 000) = 3264567774119.
;;
;; Find M(2 000 000 000).
;;

(defn blum-blum-shub-seq
  ([] (cons 290797 (blum-blum-shub-seq 290797)))
  ([n] (let [n1 (mod (* n n) 50515093)]
         (lazy-seq 
           (cons n1 (blum-blum-shub-seq n1))))))

(take 100 (reductions min (blum-blum-shub-seq)))

(defn a [i j] 
  (reduce min (take (- j i -1) (drop i (blum-blum-shub-seq)))))

(a 1 10)

(defn solve [n]
  (reduce + 
          (for [i (range 1 (inc n))
                j (range i (inc n))]
            (a i j))))

(solve 100)

(for [i (range 1 11)
      j (range i 11 )]
  [i j (a i j)])

(take 10 (blum-blum-shub-seq))


(lcm 40 32)

(defn abs "(abs n) is the absolute value of n" [n]
  (cond
   (not (number? n)) (throw (IllegalArgumentException.
			     "abs requires a number"))
   (neg? n) (- n)
   :else n))

(defn gcd 
  "(gcd a b) returns the greatest common divisor of a and b" 
  [a b]
  (if (or (not (integer? a)) (not (integer? b)))
    (throw (IllegalArgumentException. "gcd requires two integers"))  
    (loop [a (abs a) b (abs b)]
      (if (zero? b) a,
	  (recur b (mod a b))))))

(defn lcm
  "(lcm a b) returns the least common multiple of a and b"
  [a b]
  (when (or (not (integer? a)) (not (integer? b)))
    (throw (IllegalArgumentException. "lcm requires two integers")))
  (cond (zero? a) 0
        (zero? b) 0
        :else (abs (* b (quot a (gcd a b))))))
