(ns AVL-dict)

(defn node [k v left right]
  {:key   k
   :value v
   :height 1
   :left  left 
   :right right})

(defn height [node]
  (if node
    (:height node)
    0))

(defn balance-factor [node]
  (- (height (:left node)) (height (:right node))))

(defn rotate-left [node]
  (let [x (:right node)
        t2 (:left x)
        xx (assoc x :left (assoc node :right t2))] 
    (assoc node :right (assoc xx :left node))))

(defn rotate-right [node]
  (let [y (:left node)
        t3 (:right y)
        yy (assoc y :right (assoc node :left t3))] 
    (assoc node :left (assoc yy :right node))))

(defn rebalance [node]
  (let [bf (balance-factor node)]
    (cond
      (and (> bf 1) (<= (balance-factor (:left node)) 0))
      (rotate-right (assoc node :left (rotate-left (:left node))))

      (and (> bf 1) (> (balance-factor (:left node)) 0))
      (rotate-right node)

      (and (< bf -1) (>= (balance-factor (:right node)) 0))
      (rotate-left (assoc node :right (rotate-right (:right node))))

      (and (< bf -1) (< (balance-factor (:right node)) 0))
      (rotate-left node)

      :else
      node)))

;; (defn add-dict [m k v]
;;   (let [existing-value (get m k)]
;;     (assoc m k (if existing-value
;;                  (+ existing-value v)
;;                  v))))


(defn insert [tree k v]
  (cond
    (nil? tree)
    (node k v nil nil)

    (= (compare (:key tree) k) 0)
    (assoc  tree :value v)
    
    (> (compare (:key tree) k) 0)
    (node (:key tree) (:value tree) (insert (:left tree) k v) (:right tree))
    
    (< (compare (:key tree) k) 0)
    (node (:key tree) (:value tree) (:left tree) (insert (:right tree) k v)))
  
  ;;(rebalance (assoc  :height (inc (max (height (:left )) (height (:right ))))))

  )

(defn full-height
  ([tree] (full-height tree 0))
  ([tree count]
   (if tree
     (max (full-height (:left tree) (inc count))
          (full-height (:right tree) (inc count)))
     count)))

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
   (when (not (some? tree))
     (println (str tabs "NUL")))
   (when (some? tree)
     (println (str tabs (:key tree) ": " (:value tree))) 
     (to-print (str tabs "\t") (:right tree))
     (to-print (str tabs "\t") (:left tree)))))

;; (defn print-tree [node depth]
;;   (when node
;;     (doseq [_ (range (/ depth 2))]
;;       (print "\t")) ; Выводим два пробела для каждого уровня глубины
;;     (print (str (:key node) ": " (:value node)))
;;     (print "\n")
;;     (print-tree (:left node) (dec (/ depth 2)))
;;     (doseq [_ (range (/ depth 2))]
;;       (print "\t"))
;;     (print-tree (:right node) (dec (/ depth 2)))
;;     ))


(to-print (to-tree (generate-seq 5 20)))
