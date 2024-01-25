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
    (assoc Z :left (assoc tree :right Y'))))

(defn rotate-right [tree]
  (let [y (:left tree)
        t3 (:right y)]
    (assoc y :right (assoc tree :left t3))))

(defn rebalance [tree]
  (cond
    (nil? tree) tree

    (is-right-left-case? tree)
    (rotate-right (assoc tree :left (rotate-left (:left tree))))

    (is-left-right-case? tree)
    (rotate-left  (assoc tree :right (rotate-right (:right tree))))

    (is-right-case? tree)
    (rotate-right tree)

    (is-left-case? tree)
    (rotate-left tree)

    :else tree))

(defn insert [tree k v]
  (cond
    (nil? tree)
    (node k v nil nil)

    (= (compare (:key tree) k) 0)
    (assoc tree :value v)

    (> (compare (:key tree) k) 0)
    (let [new-node (node (:key tree) (:value tree) (rebalance (insert (:left tree) k v)) (:right tree))]
      (rebalance new-node))

    (< (compare (:key tree) k) 0)
    (let [new-node (node (:key tree) (:value tree) (:left tree) (rebalance (insert (:right tree) k v)))]
      (rebalance new-node))))


(defn min-find [tree]
  (cond
    (nil? tree) nil
    (nil? (:left tree)) tree

    :else (recur (:left tree))))

(defn remove-min [tree]
  (cond
    (and tree (nil? (:left tree))) (:right tree)
    :else (rebalance (node (:key tree)
                           (:value tree)
                           (remove-min (:left tree))
                           (:right tree)))))

(defn remove-elem [tree key]
  (cond
    (nil? tree)
    tree

    (> (compare (:key tree) key) 0)
    (rebalance (assoc tree :left (remove-elem (:left tree) key)))

    (< (compare (:key tree) key) 0)
    (rebalance (assoc tree :right (remove-elem (:right tree) key)))

    (nil? (:left tree)) (:right tree)
    (nil? (:right tree)) (:left tree)

    :else (let
           [min-node (min-find (:right tree))]
            (rebalance (node (:key min-node)
                             (:value min-node)
                             (:left tree)
                             (remove-min (:right tree)))))))


(defn generate-seq [n max_v]
  (let [seq1 (repeatedly n #(rand-int max_v))
        seq2 (repeatedly n #(rand-int max_v))
        seq (shuffle (zipmap seq1 seq2))]
    seq))


(defn generate-seq2 [n max_v]
  (let [seq1 (repeatedly n #(rand-int max_v))
        seq2 (repeatedly n #(rand-int max_v))
        seq11 (shuffle (zipmap seq1 seq2))
        seq12 (shuffle (zipmap seq1 seq2))]
    [seq11 seq12]))

(defn to-tree [seq]
  (reduce (fn [tree [k v]] (insert tree k v)) nil seq))

(defn add-node [tree key value]
  (insert tree key value))

(defn remove-node [tree key]
  (remove-elem tree key))

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

(defn fold-left [tree f init]
  (if (nil? tree)
    init
    (let [new-init (f init (:key tree) (:value tree))
          left-result (fold-left (:left tree) f new-init)
          right-result (fold-left (:right tree) f left-result)]
      right-result)))

(defn fold-right [tree f init]
  (if (nil? tree)
    init
    (let [new-init (f init (:key tree) (:value tree))
          right-result (fold-right (:right tree) f new-init)
          left-result (fold-right (:left tree) f right-result)]
      left-result)))

(defn merge-insert [tree1 tree2]
  (cond
    (nil? tree1) tree2
    (nil? tree2) tree2
    :else (fold-left tree1 (fn [acc k v] (add-node acc k v)) tree2)))

(defn equal-trees?
  [tree1 tree2]
  (if (and (nil? tree1) (nil? tree2))
    true
    (and (not (nil? tree1))
         (not (nil? tree2))
         (= (:key tree1) (:key tree2))
         (equal-trees? (:left tree1) (:left tree2))
         (equal-trees? (:right tree1) (:right tree2)))))


(defn tabs [n]
  (clojure.string/join (repeat n "      ")))

(defn to-print
  ([tree] (to-print "" tree))
  ([tabs tree]
   (when (not (some? tree))
     (println (str tabs "NUL")))
   (when (some? tree)
     (println (str tabs (:key tree) ": " (:value tree) " (" (height tree) ")"))
     (to-print (str tabs "\t") (:right tree))
     (to-print (str tabs "\t") (:left tree)))))

(defn visualise
  ([tree] (visualise tree 0))
  ([tree depth]
   (if tree
     (str (visualise (:right tree) (inc depth)) (tabs depth) (:key tree) ":" (:value tree) " (" (height tree) ")" "\n" (visualise (:left tree) (inc depth)))
     (str (tabs depth) "~\n"))))

(defn to-print2 [tree]
  (print (visualise tree)))

(def seq1 (generate-seq 3 30))
(print seq1)
(def xx (to-tree seq1))
(def x1 (add-node xx 20 16))
(def x2 (remove-node x1 13))

(def x3 (fold-left x1 (fn [acc k v] (+ acc v)) 0))
(def x4 (fold-right x1 (fn [acc k v] (+ acc v)) 0))

x3
x4


(to-print xx)
(to-print x1)
(to-print x2)

(def sseq  (generate-seq2 5 10))
sseq
(def seq11 (first sseq))
(def seq12 (second sseq))
(def xx1 (to-tree seq11))
(def xx2 (to-tree seq12))
(to-print xx1)
(to-print xx2)

(def x5 (merge-insert xx nil))
(def x6 (merge-insert nil xx))
(to-print xx)
(to-print x5)
(to-print x6)

(= x5 x6)

(min-find xx)

(to-print xx)

(def mapped-tree (map-tree xx (fn [k v] {:key k :value (str "Value: " (+ 15 v))})))
(to-print mapped-tree)

(def filtered-tree (filter-tree xx (fn [k v] (< k 10))))
(to-print filtered-tree)


;; add unit tests
;; add property-based test