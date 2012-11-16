;; EULER #258
;; ==========
;; A sequence is defined as:
;;
;;    g[k] = 1, for 0 <= k <= 1999
;;    g[k] = g[k-2000] + g[k-1999], for k >= 2000.
;;
;; Find g[k] mod 20092010 for k = 10^18.
;;

(ns euler258)

(defn fib [n]
  (loop [n n
         i 1N
         j 0N
         k 0N
         h 1N]
    (if (zero? n)
      j
      (if (odd? n)
        (recur
          (quot n 2) 
          (+ (* i k) (* j h))
          (+ (* i h) (* j k) (* j h))
          (+ (* k k) (* h h))
          (+ (* 2 k h) (* h h)))
        (recur
          (quot n 2) 
          i
          j
          (+ (* k k) (* h h))
          (+ (* 2 k h) (* h h)))))))

(->> (iterate inc 1)
     (map fib)
     (take 90))

(fib 1E8)
