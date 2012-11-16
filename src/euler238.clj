;; EULER #238
;; ==========
;; Create a sequence of numbers using the "Blum Blum Shub" pseudo-random 
;; number generator:
;;
;;    s[0]   = 14025256
;;    s[n+1] = s[n]^2 mod 20300713
;;
;; Concatenate these numbers s[0]s[1]s[2]... to create a string w of infinite
;; length.
;;
;; Then, w = 14025256741014958470038053646…
;;
;; For a positive integer k, if no substring of w exists with a sum of digits
;; equal to k, p(k) is defined to be zero. If at least one substring of w 
;; exists with a sum of digits equal to k, we define p(k) = z, where z is the
;; starting position of the earliest such substring.
;; 
;; For instance:
;; 
;; The substrings 1, 14, 1402, ... 
;; with respective sums of digits equal to 1, 5, 7, ...
;; start at position 1, hence p(1) = p(5) = p(7) = ... = 1.
;; 
;; The substrings 4, 402, 4025, ...
;; with respective sums of digits equal to 4, 6, 11, ...
;; start at position 2, hence p(4) = p(6) = p(11) = ... = 2.
;;
;; The substrings 02, 0252, ...
;; with respective sums of digits equal to 2, 9, ...
;; start at position 3, hence p(2) = p(9) = ... = 3.
;;
;; Note that substring 025 starting at position 3, has a sum of digits equal
;; to 7, but there was an earlier substring (starting at position 1) with a 
;; sum of digits equal to 7, so p(7) = 1, not 3.
;;
;; We can verify that, for 0 < k <= 10^3, the sum of p(k) = 4742.
;;
;; Find the sum of p(k), for 0 < k <= 2x10^15.
;;

(ns euler238
  (:use [util.primes]
        [util.misc]))

(def blum-blum-shub-seq
  (letfn [(seq0 [s] (lazy-seq (cons s (seq0 (rem (* s s) 20300713)))))]
    (seq0 14025256)))

(def w (mapcat digits blum-blum-shub-seq))

(apply str (take 40 w))

(take 100 (reductions + w))

(prime-factors-of 20300713)
(prime-factors-of 14025256)

(take 20 blum-blum-shub-seq)

(defn posn [k] 
  (->> w
       (drop (dec k))
       (filter pos?)
       (reductions +)
       (map #(vector %2 %1) (repeat k))))

(defn posn-map [maxval p] 
  (into (sorted-map) (take-while #(<= (first %) maxval) (posn p))))

(posn-map 120 1)

(take 50 (posn 1))
(take 50 (posn 2))
(take 50 (posn 3))
(take 50 (posn 4))

(defn solve [n]
  (apply (partial merge-with min)
         (for [a (range 1 (inc n))]
            (posn-map n a))))

(reduce + (vals (solve 1000)))

(vals (solve 100))
