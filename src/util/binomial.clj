(ns util.binomial)

(def pascals-triangle
  (let [seed 1N]
    (letfn [(next-row [xs] (cons seed (map (partial apply +) (partition-all 2 1 xs))))
            (rows-seq [xs] (lazy-seq (cons xs (rows-seq (next-row xs)))))]
      (rows-seq (list seed)))))

(defn choose
  "The number of ways of picking k unordered outcomes from n possibilities."
  [n k]
  (if (>= n k) 
    (-> pascals-triangle
        (nth n)
        (nth k))))

