;; EULER #396
;; ==========
;; For any positive integer n, the nth weak Goodstein sequence 
;; {g1, g2, g3, ...} is defined as:
;;
;;    g1 = n
;;    for k > 1, g[k] is obtained by writing g[k-1] in base k, 
;;    interpreting it as a base k + 1 number, and subtracting 1.
;;    
;; The sequence terminates when g[k] becomes 0.
;; 
;; For example, the 6th weak Goodstein sequence is {6, 11, 17, 25, ...}:
;;
;;    g1 = 6.
;;    g2 = 11 since 6 = 110[2], 110[3] = 12, and 12 - 1 = 11.
;;    g3 = 17 since 11 = 102[3], 102[4] = 18, and 18 - 1 = 17.
;;    g4 = 25 since 17 = 101[4], 101[5] = 26, and 26 - 1 = 25.
;;
;; and so on.
;;
;; It can be shown that every weak Goodstein sequence terminates.
;; 
;; Let G(n) be the number of nonzero elements in the nth weak Goodstein 
;; sequence. It can be verified that G(2) = 3, G(4) = 21 and G(6) = 381.
;; It can also be verified that ΣG(n) = 2517 for 1 <= n < 8.
;;
;; Find the last 9 digits of ΣG(n) for 1 <= n < 16.
;;

(ns euler396
  (:use [util.misc]))

(defn- seq0 [n k]
  (let [k-minus-one (digits n k)
        k-plus-one  (to-number k-minus-one (inc k))
        gk          (dec k-plus-one)]
    (if (zero? gk) nil (lazy-seq (cons gk (seq0 gk (inc k)))))))

(defn goodstein-seq [n]
  (cons n (seq0 n 2)))

(defn g [n]
  (count (goodstein-seq n)))

(time 
(->> (range 1 8)
     (map g)
     (reduce +)))
