;; EULER #073
;; ==========
;; Consider the fraction, n/d, where n and d are positive integers. If 
;; n<d and HCF(n,d)=1, it is called a reduced proper fraction.
;;
;; If we list the set of reduced proper fractions for d <= 8 in ascending
;; order of size, we get:
;;
;;    1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 
;;                  4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8
;;
;; It can be seen that there are 3 fractions between 1/3 and 1/2.
;; 
;; How many fractions lie between 1/3 and 1/2 in the sorted set of reduced
;; proper fractions for d <= 12,000?
;;
;; Note: The upper limit has been changed recently.
;;

(ns euler073)

(defn farey-seq [^long n]
  (letfn [(next-farey-num [^long a ^long b ^long c ^long d] 
            (lazy-seq
              (let [k (quot (+ n b) d)
                    p (- (* k c) a)
                    q (- (* k d) b)]
                (if (< c n) (cons (quot c d) (next-farey-num c d p q))))))]
    (cons 0 (next-farey-num 0 1 1 n))))

(defn solve [n]
  (count 
    (take-while #(< % (/ 1 2))
                (drop-while #(<= % (/ 1 3)) 
                            (farey-seq n)))))

(time (solve 12000))

