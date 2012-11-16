;; EULER #205
;; ==========
;; Peter has nine four-sided (pyramidal) dice, each with faces numbered 1,
;; 2, 3, 4. Colin has six six-sided (cubic) dice, each with faces numbered
;; 1, 2, 3, 4, 5, 6.
;;
;; Peter and Colin roll their dice and compare totals: the highest total
;; wins. The result is a draw if the totals are equal.
;; 
;; What is the probability that Pyramidal Pete beats Cubic Colin? Give your
;; answer rounded to seven decimal places in the form 0.abcdefg
;;

(ns euler205
  (:use [util.misc]))

(def p {:min 9 :max 36})
(def c {:min 6 :max 36})

(/ 1 (iexpt 4 9))
(/ 1 (iexpt 6 6))
