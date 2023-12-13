(ns AVL-dict)

(defn node [k v left right]
  {:key   k
   :value v
   :left  left 
   :right right})

(defn add-dict [m k v]
  (let [existing-value (get m k)]
    (assoc m k (if existing-value
                 (+ existing-value v)
                 v))))

(defn insert [tree k v]
  (cond
    (nil? tree)
    (node k v nil nil)

    (= (compare (:key tree) k) 0)
    (add-dict tree :value v)
    
    (> (compare (:key tree) k) 0)
    (node (:key tree) (:value tree) (insert (:left tree) k v) (:right tree))
    
    (< (compare (:key tree) k) 0)
    (node (:key tree) (:value tree) (:left tree) (insert (:right tree) k v)))
  )

(defn generate-seq [n max_v]
  (let [seq1 (repeatedly n #(rand-int max_v))
        seq2 (repeatedly n #(rand-int max_v))
        seq (zipmap seq1 seq2)]
    seq))

(defn to-tree [seq]
  (reduce (fn [tree [k v]] (insert tree k v)) nil seq))

(defn to-print
  ([tree] (to-print "" tree))
  ([tabs tree]
   (when (some? tree)
     (println (str tabs (:key tree) ": " (:value tree)))
     (to-print (str tabs "\t") (:left tree))
     (to-print (str tabs "\t") (:right tree)))))

(def x (to-tree (generate-seq 11 99)))

(to-print x)