(ns util.palindromes)

(set! *warn-on-reflection* true)

(defmulti is-palindrome? class)

(defmethod is-palindrome? Long [n]
  (is-palindrome? (seq (str n))))

(defmethod is-palindrome? :default [coll]
  (= coll (reverse coll)))

(defn reverse-digits [^long n]
  (loop [n n 
         res 0]
    (if (zero? n) 
      res
      (recur 
        (quot n 10) 
        (+ (* res 10) (rem n 10))))))
