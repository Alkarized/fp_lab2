(ns AVL-dict)

(defn node [k v left right]
  {:key   k
   :value v
   :left  left 
   :right right})

(defn height
  ([tree] (height tree 0))
  ([tree count]
   (if tree
     (max (height (:left tree) (inc count))
          (height (:right tree) (inc count)))
     count)))

(defn factor [tree]
  (- (height (:left tree)) (height (:right tree))))

(defn is-left-case? [tree]
  (< (factor tree) -1))

(defn is-left-right-case? [tree]
  (and (is-left-case? tree) (> (factor (:right tree)) 0)))

(defn is-right-case? [tree]
  (> (factor tree) 1))

(defn is-right-left-case? [tree]
  (and (is-right-case? tree) (< (factor (:left tree)) 0)))

(defn rotate-left [tree]
  (let [Z (:right tree)
        Y' (:left Z)]
    (assoc Z :left (assoc tree :right Y')))
  )

(defn rotate-right [tree]
    (let [y (:left tree)
          t3 (:right y)]
    (assoc y :right (assoc tree :left t3)))
  )

(defn rebalance [tree]
  (cond
    (is-right-left-case? tree) 
    (rotate-right (assoc tree :left (rotate-left (:left tree))))

    (is-left-right-case? tree) 
    (rotate-left  (assoc tree :right (rotate-right (:right tree))))

    (is-right-case? tree) 
    (rotate-right tree)

    (is-left-case? tree) 
    (rotate-left tree)

    :else tree)
  )

(defn insert [tree k v]
  (cond
    (nil? tree)
    (node k v nil nil)

    (= (compare (:key tree) k) 0)
    (assoc tree :value v)
    
    (> (compare (:key tree) k) 0)
    (let [new-node (node (:key tree) (:value tree) (insert (:left tree) k v) (:right tree))]
      (rebalance new-node))
    
    (< (compare (:key tree) k) 0)
    (let [new-node (node (:key tree) (:value tree) (:left tree) (insert (:right tree) k v))]
      (rebalance new-node)
      )
    )
  
  )



(defn generate-seq [n max_v]
  (let [seq1 (repeatedly n #(rand-int max_v))
        seq2 (repeatedly n #(rand-int max_v))
        seq (zipmap seq1 seq2)]
    seq))

(defn to-tree [seq]
  (reduce (fn [tree [k v]] (insert tree k v)) nil seq))

(defn add [tree key value]
  (insert tree key value))

(defn to-print
  ([tree] (to-print "" tree))
  ([tabs tree]
   (when (not (some? tree))
     (println (str tabs "NUL")))
   (when (some? tree)
     (println (str tabs (:key tree) ": " (:value tree) " (" (height tree) ")")) 
     (to-print (str tabs "\t") (:right tree))
     (to-print (str tabs "\t") (:left tree)))))

(defn tabs [n]
  (clojure.string/join (repeat n "      ")))

(defn visualise
  ([tree] (visualise tree 0))
  ([tree depth]
   (if tree
     (str (visualise (:right tree) (inc depth)) (tabs depth) (:key tree) ":" (:value tree) " ("(height tree)")" "\n" (visualise (:left tree) (inc depth)))
     (str (tabs depth) "~\n"))))

(defn to-print2 [tree]
  (print (visualise tree)))

(def seq1 (generate-seq 20 20))
(def xx (to-tree seq1))

(def x2 (add xx 20 16))
seq1

(to-print x2)
(to-print2 xx)

(defn map-tree [tree f]
  (if (nil? tree)
    nil
    (let [left (map-tree (:left tree) f)
          right (map-tree (:right tree) f)]
      (assoc (f (:key tree) (:value tree))
             :left left
             :right right))))

(defn filter-tree [tree predicate]
  (if (nil? tree)
    nil
    (let [left (filter-tree (:left tree) predicate)
          right (filter-tree (:right tree) predicate)]
      (if (predicate (:key tree) (:value tree))
        (assoc tree :left left :right right)
        (merge left right)))))

(def mapped-tree (map-tree xx (fn [k v] {:key k :value (str "Value: " (+ 15 v))})))
(to-print mapped-tree)

(def filtered-tree (filter-tree xx (fn [k v] (< k 10))))
(to-print filtered-tree)

;; (defn insert [tree k v]
;;   (cond
;;     (nil? tree)
;;     (node k v nil nil)

;;     (= (compare (:key tree) k) 0)
;;     (assoc tree :value v)

;;     (> (compare (:key tree) k) 0)
;;     (let [left' (insert (:left tree) k v)]
;;       (assoc tree
;;              :left left'
;;              :height (inc ((fnil max 0 0) (:height left') (:height (:right tree))))))

;;     (< (compare (:key tree) k) 0)
;;     (let [right' (insert (:right tree) k v)]
;;       (assoc tree
;;              :right right'
;;              :height (inc ((fnil max 0 0) (:height (:left tree)) (:height right')))))))