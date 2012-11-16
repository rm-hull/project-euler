;; EULER #013
;; ==========
;; Work out the first ten digits of the sum of the following 
;; one-hundred 50-digit numbers [see file 'data/50-digit-numbers.txt'].
;;

(ns euler013
  (:use [clojure.string]))

(defn add-from-file [fname]
  (reduce + (map read-string (split-lines (slurp fname)))))

(defn solve [fname]
  (.substring (str (add-from-file fname)) 0 10))

(time (solve "data/50-digit-numbers.txt"))
