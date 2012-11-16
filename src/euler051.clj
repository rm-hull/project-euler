;; EULER #052
;; ==========
;; By replacing the 1st digit of *3, it turns out that six of the nine
;; possible values: 13, 23, 43, 53, 73, and 83, are all prime.
;;
;; By replacing the 3rd and 4th digits of 56**3 with the same digit, 
;; this 5-digit number is the first example having seven primes among 
;; the ten generated numbers, yielding the family: 56003, 56113, 56333, 
;; 56443, 56663, 56773, and 56993. Consequently 56003, being the first 
;; member of this family, is the smallest prime with this property.
;;
;; Find the smallest prime which, by replacing part of the number (not 
;; necessarily adjacent digits) with the same digit, is part of an eight 
;; prime value family.
;;

(ns euler051
  (:use [util.primes]
        [util.misc]))

(defn get-repeated-digits [n num-digits]
  (->> (digits n)
       frequencies
       (filter #(= num-digits (val %)))
       (map key)))

(defn create-template [n replace-digit]
  (map #(if (not= % replace-digit) %) (digits n)))

(defn matching-primes [prime-set template]
  (filter 
    prime-set
    (for [d (range 0 10)] (to-number (map #(if (nil? %) d %) template)))))

(defn solve [num-digits replace-digits num-matches]
  (let [start (iexpt 10 (dec num-digits))
        end   (iexpt 10 num-digits)
        primes (primes-range start end)
        prime-set (set primes)]
    (first
      (for [p primes
            rd (get-repeated-digits p replace-digits)
            :let [template (create-template p rd)]
            :when (= num-matches (count (matching-primes prime-set template)))]
        p))))

(time (solve 6 3 8))

(time (solve 5 2 7))
