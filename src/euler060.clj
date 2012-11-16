;; EULER #060
;; ==========
;; The primes 3, 7, 109, and 673, are quite remarkable. By taking any two
;; primes and concatenating them in any order the result will always be 
;; prime. For example, taking 7 and 109, both 7109 and 1097 are prime. 
;; The sum of these four primes, 792, represents the lowest sum for a set
;; of four primes with this property.
;;
;; Find the lowest sum for a set of five primes for which any two primes
;; concatenate to produce another prime.
;;

(ns euler060
  (:use [util.misc]
        [util.primes]))

(def concatenated-primes
  (memoize
    (fn [^long n ^long limit]
      (let [d (digits n)]
        (set 
          (for [^long a (primes-range 2 limit)
               :when (and 
                       (is-prime? (to-number (concat d (digits a))))
                       (is-prime? (to-number (concat (digits a) d))))]
            a))))))

(defn subset [limit & ks]
  (let [s (apply clojure.set/intersection (map #(concatenated-primes % limit) ks))]
    (apply (partial disj s) ks)))

(defn solve [limit]
  (first
    (for [a (primes-range 2 limit)
          b (subset limit a)
          c (subset limit a b)
          d (subset limit a b c)
          e (subset limit a b c d)
          :let [s [a b c d e]]]
      {:sum (reduce + s) :primes s})))

(time (solve 10000))
