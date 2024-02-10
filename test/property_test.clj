(ns property-test
  {:clj-kondo/config '{:lint-as {clojure.test.check.clojure-test/defspec clojure.core/def
                                 clojure.test.check.properties/for-all clojure.core/let}}}
  (:require [clojure.test :refer [run-tests]]
            [AVL-dict :as avl]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]))

(def min-v 2) ; Минимальное возможное генерируемое число
(def max-v 10000) ; Максимально возможное генерируемое число
(def count-v 100) ; Кол-во итерация
(def size 50) ; Кол-во генерируемых значений

; Генератор для числа
(def gen-int-value
  (gen/choose min-v max-v))

; Генератор для пары
(def gen-tuple
  (gen/tuple gen-int-value gen-int-value))

; Генератор для AVL-dict
(defn gen-avl-dict [tuples]
  (reduce (fn [tree item] (avl/insert tree (first item) (second item))) nil
          tuples))

; Проверка случайного число из генерируемого списка - есть ли оно в дереве
(defspec contains-one-random-elem count-v
  (prop/for-all [tuples (gen/vector (gen/not-empty gen-tuple) size)]
                (let [tree        (gen-avl-dict tuples)
                      [key] (rand-nth tuples)]
                  (and (avl/is-valid-tree? tree) ; Валидное ли дерево
                       (avl/contains tree key)))))

; Проверка нахождение всех чисел из генератора в дереве
(defspec contains-all-elems 30
  (prop/for-all [tuples (gen/vector-distinct (gen/vector-distinct gen-int-value {:num-elements 2}) {:num-elements 1})]
                (let [tree (gen-avl-dict tuples)]
                  (and (avl/is-valid-tree? tree)
                       (every? (fn [[key value]] (= (avl/get-value tree key) value)) tuples)))))

; Проверка вставки одного элемента в дерево
(defspec insert-elem count-v
  (prop/for-all [tuples (gen/vector (gen/not-empty gen-tuple) size)]
                (let [tree        (gen-avl-dict tuples)
                      [key value] (rand-nth (gen/sample gen-tuple count-v))
                      new-avl     (avl/insert tree key value)]
                  (and (avl/is-valid-tree? new-avl)
                       (= (avl/get-value new-avl key) value)))))

; Проверка удаления одного случайно элемента из дерева 
(defspec delete-elem count-v
  (prop/for-all [tuples (gen/vector (gen/not-empty gen-tuple) size)]
                (let [tree        (gen-avl-dict tuples)
                      [key] (rand-nth tuples)
                      new-avl (avl/remove-node tree key)]
                  (and (avl/is-valid-tree? new-avl)
                       (not (avl/contains new-avl key))))))

; Проверка - будет ли дерево одним и тем же после вставки и удаления одного и того же числа
(defspec insert-delete-equals count-v
  (prop/for-all [tuples (gen/vector (gen/not-empty gen-tuple) size)]
                (let [tree        (gen-avl-dict tuples)
                      key (first (gen/sample (gen/choose (+ max-v 1) (* max-v 3)) 1))
                      value (first (gen/sample gen-int-value))
                      new-avl-insert (avl/insert tree key value)
                      new-avl-delete (avl/remove-node tree key)]
                  (and (avl/is-valid-tree? new-avl-insert)
                       (avl/contains new-avl-insert key)
                       (not (avl/contains new-avl-delete key))
                       (= new-avl-delete tree)))))

; Проверка сверток (левой и правой) - находится сумма всех ключей дерева и сравнивается с суммой ключей генерируемых данных
(defspec fold-clear count-v
  (prop/for-all [tupels (gen/vector-distinct (gen/vector-distinct gen-int-value {:num-elements 2}) {:num-elements 1})]
                (let [tree        (gen-avl-dict tupels)
                      fold-left-sum      (avl/fold-left tree (fn [acc k v] (+ acc k (* v 0))) 0)
                      fold-right-sum     (avl/fold-right tree (fn [acc k v] (+ acc k (* v 0))) 0)
                      true-sum (reduce (fn [acc [k v]] (+ acc k (* v 0))) 0 tupels)]
                  (and (avl/is-valid-tree? tree)
                       (= fold-left-sum fold-right-sum true-sum)))))

; Проверка, что после соединения 2ух деревьев, новое дерево содержит элементы обоих деревьев
(defspec merge-contains count-v
  (prop/for-all [tuples1 (gen/vector (gen/not-empty gen-tuple))
                 tuples2 (gen/vector (gen/not-empty gen-tuple))]
                (let [tree1        (gen-avl-dict tuples1)
                      tree2        (gen-avl-dict tuples2)
                      merge (avl/merge-insert tree1 tree2)
                      tuples (concat tuples1 tuples2)]
                  (and (avl/is-valid-tree? merge)
                       (every? (fn [[key]] (avl/contains merge key)) tuples)))))

; Проверка работы фильтрации, фильтруется дерево и генерируемые данные, на основе генерируемых данных создается новое дерево и сравнивается с полученным ранее
(defspec filter-elem count-v
  (prop/for-all [tupels (gen/vector-distinct (gen/vector-distinct gen-int-value {:num-elements 2}) {:num-elements 1})]
                (let [tree (gen-avl-dict tupels)
                      filtered-tree (avl/filter-tree tree (fn [k v] (even? (+ k (* 0 v)))))
                      filtered-sorted-tuple (filter (fn [[k]] (even? k)) tupels)
                      filtered-tree-expected (gen-avl-dict filtered-sorted-tuple)]
                  (and (avl/is-valid-tree? filtered-tree)
                       (avl/equal-trees? filtered-tree filtered-tree-expected)))))

; Проверка свойства моноида - единичный элемент
(defspec empty-monoid count-v
  (prop/for-all [tuples (gen/vector (gen/not-empty gen-tuple) size)]
                (let [tree        (gen-avl-dict tuples)
                      empty-element nil
                      new-avl     (avl/merge-insert tree empty-element)]
                  (and (avl/is-valid-tree? new-avl)
                       (= tree new-avl)))))

; Проверка свойства моноида - ассоциативность
(defspec merge-monoid 10
  (prop/for-all [tuples1 (gen/vector (gen/not-empty gen-tuple))
                 tuples2 (gen/vector (gen/not-empty gen-tuple))
                 tuples3 (gen/vector (gen/not-empty gen-tuple))]
                (let [tree1        (gen-avl-dict tuples1)
                      tree2        (gen-avl-dict tuples2)
                      tree3        (gen-avl-dict tuples3)
                      merge1 (avl/merge-insert (avl/merge-insert tree1 tree2) tree3)
                      merge2 (avl/merge-insert tree1 (avl/merge-insert tree2 tree3))]
                  (and (avl/is-valid-tree? merge1)
                       (avl/is-valid-tree? merge2)
                       (avl/equal-trees? merge1 merge2)))))

(run-tests)