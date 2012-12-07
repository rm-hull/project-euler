(ns util.dijkstra
 (:use [clojure.string :only (split-lines split)]))

(defmacro add-if [pred then-clause xs]
  `(if ~pred
     (cons ~then-clause ~xs)
     ~xs))

(defn get-data 
  "Reads in the matrix data into a flat vector, returns a map
   with :data and :size keys"
  [fname]
  (let [raw-data (split-lines (slurp fname))
        parse-csv (fn [x] (map read-string (split x #",")))]
    { :data (vec (mapcat parse-csv raw-data))
      :size [ (count (parse-csv (first raw-data))) (count raw-data) ] }))

(defn weight [matrix p]
  (get-in matrix [:data p]))

(defn connecting-neighbours [matrix neighbours-fn p]
  (->> (neighbours-fn p (:size matrix))
       (map #(vector % (weight matrix %)))
       (into {})))

(defn- remove-longer-paths
  "Filters out those neighbours from the predecessors whose length is greater
   than the current length"
  [pred neighbours-weights curr-len]
  (letfn [(rm-func [[neighbour weight]] (if-let [old (pred neighbour)] (>= (+ curr-len weight) (:length old))))]
    (remove rm-func neighbours-weights)))
 
(defn- path-length
  "Given a map of predecessors of a specific start point, extract out the
   length for the cell at offset n."
  [pred n]
  (get-in pred [n :length] 0))

(defn- blend-in
  "Blends the neighbours (as keys) into the map all with the value derived
   from the val-func given the neighbour."
  [map neighbours val-func]
  (if (empty? neighbours)
    map
    (apply (partial assoc map) (mapcat #(vector (first %) (val-func %)) neighbours))))
 
(defn build-predecessors
  "Constructs a map of predessors for cells between 'start' and 'stop-at'
   cells."
  [matrix neighbours-fn start stop-at]
  (loop [pred (hash-map start { :predecessor nil :length (weight matrix start) })
         active (sorted-map start (weight matrix start))]
    (cond
      (empty? active) pred
      (= (first active) stop-at) pred
      :else (let [curr (ffirst active)
                  curr-length (path-length pred curr)
                  neighbours (remove-longer-paths pred (connecting-neighbours matrix neighbours-fn curr) curr-length)
                  next-gen (blend-in pred neighbours #(hash-map :predecessor curr :length (+ curr-length (second %))))
                  next-active (blend-in active neighbours #(+ curr-length (second %)))]
              (recur
                next-gen
                (dissoc next-active curr))))))

(defn get-path [to predecessors]
  (loop [n to
         result nil]
    (let [p (get-in predecessors [n :predecessor])]
      (if (nil? p)
        (vec (cons n result))
        (recur p (cons n result))))))

(defn shortest-path
  "Recurses over the predecessors between 'from' and 'to' pulling out the
   cells into an ordered list which represents the shortest path between
   the two points."
  [matrix neighbours-fn from to]
  (get-path to
    (build-predecessors matrix neighbours-fn from to)))
