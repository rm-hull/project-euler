;; EULER #107
;; ==========
;; The following undirected network consists of seven vertices and twelve edges with a total weight of 243.
;;
;;          B     20      E
;;           +-----------+
;;          / \         / \
;;         /   \       /   \
;;     16 /     \     /     \ 11
;;       /    17 \   / 18    \
;;      /         \ /         \
;;   A +-----------+-----------+ G
;;      \   21    /D\    23   /
;;       \       /   \       /   
;;     12 \     /     \ 19  /     
;;         \   / 28    \   / 24
;;          \ /         \ /
;;           +-----------+
;;          C     31      F
;;
;; The same network can be represented by the matrix below.
;;
;;         A   B   C   D   E   F   G
;;    A   -   16   12  21  -   -   -
;;    B   16  -    -   17  20  -   -
;;    C   12  -    -   28  -   31  -
;;    D   21  17   28  -   18  19  23
;;    E   -   20   -   18  -   -   11
;;    F   -   -    31  19  -   -   27
;;    G   -   -    -   23  11  27  -
;;
;; However, it is possible to optimise the network by removing some edges
;; and still ensure that all points on the network remain connected. The 
;; network which achieves the maximum saving is shown below. It has a 
;; weight of 93, representing a saving of 243 - 93 = 150 from the original
;; network.
;;
;;          B             E
;;           +           +
;;          / \         / \
;;         /   \       /   \
;;     16 /     \     /     \ 11
;;       /    17 \   / 18    \
;;      /         \ /         \
;;   A +           +           + G
;;      \         D \
;;       \           \
;;     12 \           \ 19
;;         \           \
;;          \           \
;;           +           +
;;          C             F
;;
;; Using 'data/network.txt', a 6K text file containing a network with forty
;; vertices, and given in matrix form, find the maximum saving which can be
;; achieved by removing redundant edges whilst ensuring that the network
;; remains connected.
;;

(ns euler107
  (:use [clojure.string :only (split-lines split)]
        [clojure.data.priority-map]))

(defn get-adjacency-matrix [fname]
  (let [raw-data (split-lines (slurp fname))
        parse-csv (fn [x] (vec (map read-string (split x #","))))]
     (vec (map parse-csv raw-data))))

(defn vertices [matrix] 
  (set (range 0 (count matrix))))

(defn edges [matrix]
  (into (priority-map) 
    (for [u (vertices matrix)
          v (range u (count matrix))
          :let [weight (get-in matrix [u v])]
          :when (not= weight '-)]
        [[u v] weight])))

(defn weight [edges]
  (reduce + (vals edges)))

(defn pick-edge [edges visited]
  (letfn [(pred [[[u v] _]] (or (nil? (visited u)) (visited v)))]
    (first
      (drop-while pred edges))))

(defn minimum-spanning-tree [vertices edges]
  (loop [visited  (hash-set (first vertices))
         vertices (disj vertices (first visited))
         edges    edges 
         res      nil]
    (if (empty? vertices)
      res
      (let [[a b] (pick-edge edges visited)]
        (recur 
          (conj visited (second a))
          (disj vertices (second a))
          (dissoc edges a)
          (assoc res a b))))))

(defn solve [fname]
  (let [matrix   (get-adjacency-matrix fname)
        vertices (vertices matrix)
        edges    (edges matrix)] 
    (- (weight edges)
       (weight (minimum-spanning-tree vertices edges)))))

(time (solve "data/7-network.txt"))

(time (solve "data/40-network.txt"))

(def m (get-adjacency-matrix "data/40-network.txt"))

(weight  (edges m)) 
