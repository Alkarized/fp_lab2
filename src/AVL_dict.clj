(ns AVL-dict
  (:require [clojure.string :as str]))

; Создание элемента дерева (корень/потомок)
(defn node [k v left right]
  {:key   k
   :value v
   :left  left
   :right right})

; Поиск высоты
(defn height
  ([tree] (height tree 0))
  ([tree count]
   (if tree
     (max (height (:left tree) (inc count))
          (height (:right tree) (inc count)))
     count)))

; Главынй фактор абсолютная разница высот соседних вершин не превышает 1,  
(defn factor [tree]
  (- (height (:left tree)) (height (:right tree))))

; Левый случай. P.S Случай - проверка, надо ли нам перестравить дерево, что бы удовлетворяло условиям построения AVL
(defn is-left-case? [tree]
  (< (factor tree) -1))

; Левый-правый случай
(defn is-left-right-case? [tree]
  (and (is-left-case? tree) (> (factor (:right tree)) 0)))

; Правый случай
(defn is-right-case? [tree]
  (> (factor tree) 1))

; Правый-левый случай
(defn is-right-left-case? [tree]
  (and (is-right-case? tree) (< (factor (:left tree)) 0)))

; Левый поворот
(defn rotate-left [tree]
  (let [Z (:right tree)
        Y' (:left Z)]
    (assoc Z :left (assoc tree :right Y'))))

; Правый поворт
(defn rotate-right [tree]
  (let [y (:left tree)
        t3 (:right y)]
    (assoc y :right (assoc tree :left t3))))

; Проверка, необходимо ли нам повернуть как-то часть дерева, что бы оно было AVL
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

; Вставка в дерево
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

; Поиск минимального элемента
(defn min-find [tree]
  (cond
    (nil? tree) nil
    (nil? (:left tree)) tree

    :else (recur (:left tree))))

; Вспомогательная функция - удаление минимального элемента 
(defn remove-min [tree]
  (cond
    (and tree (nil? (:left tree))) (:right tree)
    :else (rebalance (node (:key tree)
                           (:value tree)
                           (remove-min (:left tree))
                           (:right tree)))))

; Удаление элемента из дерева
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

; Вспомогательная функция - генерация последовательности
(defn generate-seq [n max_v]
  (let [seq1 (repeatedly n #(rand-int max_v))
        seq2 (repeatedly n #(rand-int max_v))
        seq (zipmap seq1 seq2)]
    seq))

; Вспомогательная функция - генерация последовательности
(defn generate-seq2 [n max_v]
  (let [seq1 (repeatedly n #(rand-int max_v))
        seq2 (repeatedly n #(rand-int max_v))
        seq11 (shuffle (zipmap seq1 seq2))
        seq12 (shuffle (zipmap seq1 seq2))]
    [seq11 seq12]))

; Создание дерева из последовательности
(defn to-tree [seq]
  (reduce (fn [tree [k v]] (insert tree k v)) nil seq))

; Обертка над удалением
(defn remove-node [tree key]
  (remove-elem tree key))

; Функция применения какой-то другой функции для всех элементов дерева
(defn map-tree [tree f]
  (if (nil? tree)
    nil
    (let [left (map-tree (:left tree) f)
          right (map-tree (:right tree) f)]
      (assoc (f (:key tree) (:value tree))
             :left left
             :right right))))

; Левая свертка
(defn fold-left [tree f init]
  (if (nil? tree)
    init
    (let [new-init (f init (:key tree) (:value tree))
          left-result (fold-left (:left tree) f new-init)
          right-result (fold-left (:right tree) f left-result)]
      right-result)))

; Правая свертка
(defn fold-right [tree f init]
  (if (nil? tree)
    init
    (let [new-init (f init (:key tree) (:value tree))
          right-result (fold-right (:right tree) f new-init)
          left-result (fold-right (:left tree) f right-result)]
      left-result)))

; Соединение (слияние) двух деревьев в одно единое 
(defn merge-insert [tree1 tree2]
  (cond
    (nil? tree1) tree2
    (nil? tree2) tree1
    :else (fold-left tree1 (fn [acc k v] (insert acc k v)) tree2)))

; Фильтрация элементов дерева
(defn filter-tree [tree predicate]
  (if (nil? tree)
    nil
    (let [left (filter-tree (:left tree) predicate)
          right (filter-tree (:right tree) predicate)]
      (if (predicate (:key tree) (:value tree))
        (rebalance (assoc tree :left left :right right))
        (rebalance (merge-insert left right))))))

; Превращение дерева в отсортированный список
(defn to-sorted-list [tree]
  (if (nil? tree) '()
      (concat (to-sorted-list (:left tree))
              [(:key tree) (:value tree)]
              (to-sorted-list (:right tree)))))

; Провека деревьев на содержание всех одинаковых элементов
(defn equal-trees?
  [tree1 tree2]
  (= (to-sorted-list tree1) (to-sorted-list tree2)))

; Вспомогательная функция для вывода пробелом
(defn tabs [n]
  (str/join (repeat n "      ")))

; Вставка последовательности в дерево
(defn insert-sequence [tree sequence]
  (reduce (fn [acc [k v]] (insert acc k v)) tree sequence))

; Вывод дерева - 1ый вариант
(defn to-print
  ([tree] (to-print "" tree))
  ([tabs tree]
   (when (not (some? tree))
     (println (str tabs "NUL")))
   (when (some? tree)
     (println (str tabs (:key tree) ": " (:value tree) " (" (height tree) ")"))
     (to-print (str tabs "\t") (:right tree))
     (to-print (str tabs "\t") (:left tree)))))

; Всопомгательаня функция для вывода дерева (2ой вариант)
(defn visualise
  ([tree] (visualise tree 0))
  ([tree depth]
   (if tree
     (str (visualise (:right tree) (inc depth)) (tabs depth) (:key tree) ":" (:value tree) " (" (height tree) ")" "\n" (visualise (:left tree) (inc depth)))
     (str (tabs depth) "~\n"))))

; Обертка над выводом дерева - 2ой вариант
(defn to-print2 [tree]
  (print (visualise tree)))

; Получение значения из дерева по ключу
(defn get-value [tree key]
  (cond
    (nil? tree) nil
    (= (compare (:key tree) key) 0)
    (:value tree)

    (> (compare (:key tree) key) 0)
    (get-value (:left tree) key)
    (< (compare (:key tree) key) 0)
    (get-value (:right tree) key)))

; Проверка, содержит ли данное дерево ключ
(defn contains [tree key]
  (if (nil? (get-value tree key)) false true))

; Вспомогательная функция - сбалансировано ли дерево - выполняется ли условия построения AVL-дерева
(defn is-balanced? [tree]
  (if (nil? tree) true
      (let [left-h (height (:left tree))
            right-h (height (:right tree))]
        (and (<= (Math/abs (- left-h right-h)) 1)
             (is-balanced? (:left tree))
             (is-balanced? (:right tree))))))

; Вспомогательная функция - бинарное ли дерево
(defn binary-helper [node min-value max-value] ;; Работает только с целыми значениями в key
  (if (nil? node) true
      (and (and (<= (compare min-value (:key node)) 0) (>= (compare max-value (:key node)) 0))
           (binary-helper (:left node) min-value (:key node))
           (binary-helper (:right node) (:key node) max-value))))

; Открытая обертка для проверки - является ли дерево бинарным
(defn is-binary? [tree]
  (binary-helper tree Long/MIN_VALUE Long/MAX_VALUE))

; Валидное ли дерево - бинарное и сбалансированное? 
(defn is-valid-tree? [tree]
  (and (is-binary? tree) (is-balanced? tree)))

;; Вспомогательная функция для проверки, что в дереве есть все элементы из данного списка
(defn check-itmes [tree seq]
  (every? (fn [[k v]] (= v (get-value tree k))) seq))

;; (def seq1 (generate-seq 3 30))
;; (print seq1)
;; (def xx (to-tree seq1))
;; (def x1 (add-node xx 20 16))
;; (def x2 (remove-node x1 13))

;; (def x3 (fold-left x1 (fn [acc k v] (+ acc v (* k 0))) 0))
;; (def x4 (fold-right x1 (fn [acc k v] (+ acc v (* k 0))) 0))

;; x3
;; x4

;; (to-print xx)
;; (to-print x1)
;; (to-print x2)

;; (print (get-value x2 12))

;; (def sseq  (generate-seq2 5 10))
;; sseq
;; (def seq11 (first sseq))
;; (def seq12 (second sseq))
;; (def xx1 (to-tree seq11))
;; (def xx2 (to-tree seq12))
;; (to-print xx1)
;; (to-print xx2)

;; (def x5 (merge-insert xx nil))
;; (def x6 (merge-insert nil xx))
;; (to-print xx)
;; (to-print x5)
;; (to-print x6)

;; (= x5 x6)

;; (min-find xx)

;; (to-print xx)

;; (def mapped-tree (map-tree xx (fn [k v] {:key k :value (str "Value: " (+ 15 v))})))
;; (to-print mapped-tree)

;; (def filtered-tree (filter-tree xx (fn [k v] (and (< k 10) v))))
;; (to-print filtered-tree)

;; add unit tests
;; add property-based test