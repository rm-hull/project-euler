(ns util.fibonacci
  (:use [util.misc]))

(def fibonacci-seq
  (letfn [(seq0 [a b] (lazy-seq (cons a (seq0 b (+ a b)))))]
    (seq0 1N 1N)))

(def tribonacci-seq
  "The tribonacci numbers are like the Fibonacci numbers, but instead of 
   starting with two predetermined terms, the sequence starts with three
   predetermined terms and each term afterwards is the sum of the preceding
   three terms."
  (letfn [(seq0 [a b c] (lazy-seq (cons a (seq0 b c (+ a b c)))))]
    (seq0 1N 1N 1N)))

(def golden-ratio 
  (/ (inc (Math/sqrt 5)) 2))

(defn fib [n]
  (bigint (+
            (/ 
              (iexpt (bigdec golden-ratio) n)
              (Math/sqrt 5))
            0.5)))

