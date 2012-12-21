;; EULER #088
;; ==========
;; A natural number, N, that can be written as the sum and product of a 
;; given set of at least two natural numbers, {a[1], a[2], ... , a[k]} is
;; called a product-sum number: N = a[1] + a[2] + ... + a[k] = 
;; a[1] x a[2] x ... x a[k].
;;
;; For example, 6 = 1 + 2 + 3 = 1 x 2 x 3.
;;
;; For a given set of size, k, we shall call the smallest N with this
;; property a minimal product-sum number. The minimal product-sum numbers
;; for sets of size, k = 2, 3, 4, 5, and 6 are as follows.
;;
;;    k=2: 4 = 2 x 2 = 2 + 2
;;    k=3: 6 = 1 x 2 x 3 = 1 + 2 + 3
;;    k=4: 8 = 1 x 1 x 2 x 4 = 1 + 1 + 2 + 4
;;    k=5: 8 = 1 x 1 x 2 x 2 x 2 = 1 + 1 + 2 + 2 + 2
;;    k=6: 12 = 1 x 1 x 1 x 1 x 2 x 6 = 1 + 1 + 1 + 1 + 2 + 6
;;
;; Hence for 2<=k<=6, the sum of all the minimal product-sum numbers is 
;; 4+6+8+12 = 30; note that 8 is only counted once in the sum.
;;
;; In fact, as the complete set of minimal product-sum numbers for 
;; 2<=k<=12 is {4, 6, 8, 12, 15, 16}, the sum is 61.
;;
;; What is the sum of all the minimal product-sum numbers for 2<=k<=12000?
;;

(ns euler088
  (:use [util.combinatorics]))

(defn fill [xs k]
  (let [size (count xs)]
    (if (>= size k)
      xs
      (concat (repeat (- k size) 1) xs))))

(defn expand [k n]
  (mapcat #(repeat n %) (range 2 (inc k))))

(defn product-sum [xs]
  { :set xs :product (reduce * xs) :sum (reduce + xs) })

(defn product-sum? [m]
  (= (reduce * m) (reduce + m)))

(defn in-range? [lower upper xs]
  (let [res (reduce * xs)]
    (and 
      (>= res lower)
      (<= res upper))))

(defn all-combinations [k]
  (let [ks (range 1 (inc k))]
    (->>
      (mapcat #(combinations (expand k %) %) ks)
      (map #(fill % k))
      (filter (partial in-range? k (* k 2)))
      distinct)))

(->>
  (all-combinations 8)
  reverse
  (filter product-sum?)
  first
  (apply +)
)

(expand 6 3)

(distinct (combinations (list 2 2 2 3 3 3 4 4 4 5 5 5 6 6 6) 3))

(distinct (map #(fill % 6) (combinations (expand 6 2) 2)))

(distinct (map #(fill % 6) (combinations (expand 6 3) 3)))

(distinct (map #(fill % 6) (combinations (expand 6 4) 4)))

(distinct (map #(fill % 6) (combinations (expand 6 6) 6)))

(fill (list 2 2 2) 6)

(fill (list 2 2 2) 6)

(expand 6 3)

(all-combinations 5)



(def partitions 
  (memoize
    (fn [n limit]
      (if (zero? n)
        [[]]
        (for [x (range (min n limit) 0 -1)
              p (partitions (- n x) x)]
          (cons x p))))))


(defn partitions 
  ([n] (partitions n n)) 
  ([n limit]
    (if (zero? n)
      [[]]
      (for [x (range (min n limit) 0 -1)
            p (partitions (- n x) x)]
        (cons x p)))))



(defn product-sum [n]
  (->> 
    (partitions n n)
    rest
    (filter #(= n (reduce * %)))
    (map #(vector (count %) %))
    (into {})
    ))


(->>
  (range 4 100 2)
  (map product-sum)
  )

(product-sum 4)
(product-sum 6)
(product-sum 8)
(product-sum 10)
(product-sum 12)
(product-sum 14)
(product-sum 16)
(product-sum 18)
(product-sum 20)
(product-sum 22)
(product-sum 24)
(product-sum 26)
(product-sum 28)
(product-sum 30)
(product-sum 32)
(product-sum 34)



(partitions 5 4)

(permutations '(2 2 1))
