;; EULER #171
;; ==========
;; For a positive integer n, let f(n) be the sum of the squares of the 
;; digits (in base 10) of n, e.g.
;;
;;    f(3) = 3^2 = 9,
;;    f(25) = 2^2 + 5^2 = 4 + 25 = 29,
;;    f(442) = 4^2 + 4^2 + 2^2 = 16 + 16 + 4 = 36
;;
;; Find the last nine digits of the sum of all n, 0 < n < 10^20, such 
;; that f(n) is a perfect square.
;;

(ns euler171
  (:use [util.misc]))


(def sq (into #{} (take-while #(< % 1e8) squares)))

(sq 36)



(def A003132 
  "Sum of squares of digits of n. It is easy to show that 
   a(n) < 81*(log(n)+1) [log = base 10]."
  (letfn [(g [n] (reduce + (map #(* % %) (digits n))))]
    (map g integers)))

(take 40 A003132)

(->> (map vector integers A003132)
     (filter #(sq (second %)))
     (take 1000)
     (group-by second))
