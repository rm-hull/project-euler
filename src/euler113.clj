;; EULER #113
;; ==========
;; Working from left-to-right if no digit is exceeded by the digit to its
;; left it is called an increasing number; for example, 134468.
;;
;; Similarly if no digit is exceeded by the digit to its right it is called
;; a decreasing number; for example, 66420.
;;
;; We shall call a positive integer that is neither increasing nor 
;; decreasing a "bouncy" number; for example, 155349.
;;
;; As n increases, the proportion of bouncy numbers below n increases such
;; that there are only 12951 numbers below one-million that are not bouncy
;; and only 277032 non-bouncy numbers below 10^10.
;;
;; How many numbers below a googol (10^100) are not bouncy?
;;

(ns euler113)

(def pascals-triangle
  (let [seed 1N]
    (letfn [(next-row [xs] (cons seed (map (partial apply +) (partition-all 2 1 xs))))
            (rows-seq [xs] (lazy-seq (cons xs (rows-seq (next-row xs)))))]
      (rows-seq (list seed)))))

(defn choose
  "The number of ways of picking k unordered outcomes from n possibilities."
  [n k]
  (if (> n k) 
    (-> pascals-triangle
        (nth n)
        (nth k))))

(defn solve [n]
  (long 
    (+ 
      (choose (+ n 9) n)
      (choose (+ n 10) n)
      (* -10 n)
      -2)))

(time (solve 100))
