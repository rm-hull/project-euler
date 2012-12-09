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

(defn edges 
  "Collects all uni-directional edges between all vertices in the matrix"
  [matrix]
  (into (priority-map) 
    (for [u (vertices matrix)
          v (vertices matrix)
          :let [weight (get-in matrix [v u])]
          :when (not= weight '-)]
        [[u v] weight])))

(defn weight 
  "Totals the weight between all vertices; if bi-directional edges are
   supplied it will be necessary to divide by two to get the correct
   network weight."
  [edges]
  (reduce + (vals edges)))

(defn pick-edge 
  "Picks the first edge {u,v} with minimum weight where u has been visited
   previously, but v has not"
  [edges visited]
  (letfn [(pred [[[u v] _]] (and (visited u) (nil? (visited v))))]
    (first
      (filter pred edges))))

(defn minimum-spanning-tree 
  "Prim's algorithm for calculating the minimum spanning tree given a 
   list of connected vertices and their edges."
  [vertices edges]
  (loop [visited  (hash-set  (first vertices))
         res      (priority-map)]
    (if (= vertices visited)
      res
      (let [[a b] (pick-edge edges visited)]
        (recur 
          (conj visited (second a))
          (assoc res a b))))))

(defn solve [fname]
  (let [matrix   (get-adjacency-matrix fname)
        vertices (vertices matrix)
        edges    (edges matrix)] 
    (- (quot (weight edges) 2) ; divide by 2 because bi-directional weights
       (weight (minimum-spanning-tree vertices edges)))))

(time (solve "data/7-network.txt"))

(time (solve "data/40-network.txt"))
