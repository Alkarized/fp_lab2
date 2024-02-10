# Лабораторная работа №2

## Цель работы: 
Освоиться с построением пользовательских типов данных, полиморфизмом, рекурсивными алгоритмами и средствами тестирования (unit testing, property-based testing).

## Требования

1. Функции:

* добавление и удаление элементов;
* фильтрация;
* отображение (map);
* свертки (левая и правая);
* структура должна быть моноидом.


2. Структуры данных должны быть неизменяемыми.
3. Библиотека должна быть протестирована в рамках unit testing.
4. Библиотека должна быть протестирована в рамках property-based тестирования (как минимум 3 свойства, включая свойства моноида).
5. Структура должна быть полиморфной.
6. Требуется использовать идиоматичный для технологии стиль программирования. Примечание: некоторые языки позволяют получить большую часть API через реализацию небольшого интерфейса. Так как лабораторная работа про ФП, а не про экосистему языка -- необходимо реализовать их вручную и по возможности -- обеспечить совместимость.

## Вариант:

AVL-dict - Clojure

## Выпполнение

```Clojure
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
...

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
```

Небольшой кусочек кода, который описывает структуру AVL-dict, а так же реализацию вставки элемента в дерево

Остальной код можно найти [тут](./fp-lab2/src/AVL_dict.clj)

## Unit-тестирование

```Clojure
; Проверки, что дерево содержит/не содержит элемент
(deftest contain
  (testing "contains"
    (let [tree (avl/to-tree [[412 123]])]
      (is (avl/contains tree 412))))
  (testing "not-contains"
    (let [tree (avl/to-tree [[412 123]])]
      (is (not (avl/contains tree 1))))))

; Проверка вставки
(deftest insert
  (testing "inserting-item-in-empty-tree" ; Вставка в пустое дерево одного элемента
    (let [tree-nil nil
          expected-tree (avl/node 4 2 nil nil)
          new-tree (avl/insert tree-nil 4 2)]

      (is (= expected-tree new-tree))
      (is (avl/is-valid-tree? new-tree)))) ; Проверка валидности дерева

  (testing "inserting-item-in-tree-with-valid-check-int-keys-int-values" ;  Создание дерева с типом данных интов у ключей и значени
    (let [tree-base (avl/to-tree (map vector keys-seq values-seq))
          tree-expected (avl/to-tree (map vector (conj keys-seq 73) (conj values-seq 41)))
          tree-added (avl/insert tree-base 73 41)]

      (is (avl/is-valid-tree? tree-added))
      (is (= tree-added tree-expected))))

  (testing "inserting-item-in-tree-with-balance-check-string-keys-int-values" ;  Создание дерева с типом данных ключей в виде строк, значений в виде интов
    (let [tree-base (avl/to-tree (map vector (map str keys-seq) values-seq))
          tree-expected (avl/to-tree (map vector (map str (conj keys-seq 73)) (conj values-seq 41)))
          tree-added (avl/insert tree-base "73" 41)]

      (is (avl/is-balanced? tree-added))
      (is (= tree-added tree-expected))))

  (testing "inserting-item-in-tree-with-balance-check-string-keys-string-values" ;  Создание дерева с типом данных ключей и значений в виде строк
    (let [tree-base (avl/to-tree (map vector (map str keys-seq) (map str values-seq)))
          tree-expected (avl/to-tree (map vector (map str (conj keys-seq 73)) (map str (conj values-seq 41))))
          tree-added (avl/insert tree-base "73" "41")]

      (is (avl/is-balanced? tree-added))
      (is (= tree-added tree-expected))))

  (testing "inserting-item-in-tree-with-balance-check-int-keys-string-values" ; Создание дерева с типом данных значений в виде строк, ключей в виде интов
    (let [tree-base (avl/to-tree (map vector keys-seq (map str values-seq)))
          tree-expected (avl/to-tree (map vector (conj keys-seq 73) (map str (conj values-seq 41))))
          tree-added (avl/insert tree-base 73 "41")]

      (is (avl/is-valid-tree? tree-added))
      (is (= tree-added tree-expected))))

  (testing "inserting-items-in-tree" ; Проверка, что после вставки элемента в дерево он там есть
    (let [tree-base  (avl/to-tree seq1)
          tree-expected (avl/to-tree (concat seq1 seq2))
          tree-actual (avl/insert-sequence tree-base seq2)]

      (is (avl/equal-trees? tree-actual tree-expected))
      (is (avl/is-valid-tree? tree-actual)))))
```

Тут приведен небольшой фрагмент реализации ***unit*** тестов для вставки элемента

Полное содержание ***unit*** тестов можно найти [тут](./fp-lab2/test/units_test.clj)

## Property-based тестирование

```Clojure
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
```

Для ***property-based*** тестирования использовался модуль: ``org.clojure/test.check``

В данном фрагменте приведен основной генератор чисел для тестов, а так же пример реализации теста: содержит ли дерево случайный элемент из сгенерированной последовательности, которая в свою очередь пораждает это дерево.

Полное содержание ***property-based***  тестов можно найти [тут](./fp-lab2/test/property_test.clj)

## Выходные данные тестирования

```
Testing property-test
{:result true, :num-tests 100, :seed 1707533023541, :time-elapsed-ms 303, :test-var "insert-delete-equals"}
{:result true, :num-tests 100, :seed 1707533023851, :time-elapsed-ms 200, :test-var "merge-contains"}
{:result true, :num-tests 100, :seed 1707533024052, :time-elapsed-ms 16, :test-var "fold-clear"}
{:result true, :num-tests 10, :seed 1707533024069, :time-elapsed-ms 4, :test-var "merge-monoid"}
{:result true, :num-tests 100, :seed 1707533024074, :time-elapsed-ms 125, :test-var "insert-elem"}
{:result true, :num-tests 100, :seed 1707533024200, :time-elapsed-ms 121, :test-var "empty-monoid"}
{:result true, :num-tests 100, :seed 1707533024323, :time-elapsed-ms 84, :test-var "delete-elem"}
{:result true, :num-tests 100, :seed 1707533024408, :time-elapsed-ms 7, :test-var "filter-elem"}
{:result true, :num-tests 30, :seed 1707533024416, :time-elapsed-ms 3, :test-var "contains-all-elems"}
{:result true, :num-tests 100, :seed 1707533024420, :time-elapsed-ms 85, :test-var "contains-one-random-elem"}

Ran 10 tests containing 10 assertions.
0 failures, 0 errors.

Testing units-test

Ran 9 tests containing 39 assertions.
0 failures, 0 errors.
--- unit (clojure.test) ---------------------------
units-test
  foldsok units-test/folds (units_test.clj:113)

    fold-left-sum-valuesok units-test/folds (units_test.clj:119)

    fold-right-sum-keys
  map-avlok units-test/map-avl (units_test.clj:148)

    apply-x2-to-valuesok units-test/map-avl (units_test.clj:149)
ok units-test/map-avl (units_test.clj:150)
ok units-test/map-avl (units_test.clj:159)

    apply-x10-to-keysok units-test/map-avl (units_test.clj:160)
ok units-test/map-avl (units_test.clj:161)

  deleteok units-test/delete (units_test.clj:85)

    delete-item-from-tree-with-balance-checkok units-test/delete (units_test.clj:86)
ok units-test/delete (units_test.clj:87)
ok units-test/delete (units_test.clj:93)

    delete-item-from-tree-with-balance-check-where-no-this-itemok units-test/delete (units_test.clj:94)
ok units-test/delete (units_test.clj:95)

  monoidok units-test/monoid (units_test.clj:170)

    check-empty-treeok units-test/monoid (units_test.clj:171)
ok units-test/monoid (units_test.clj:172)

  filter-avlok units-test/filter-avl (units_test.clj:128)

    filter-by-even-nums-in-valuesok units-test/filter-avl (units_test.clj:129)
ok units-test/filter-avl (units_test.clj:136)

    filter-by-odd-nums-in-keysok units-test/filter-avl (units_test.clj:137)

  merge-testok units-test/merge-test (units_test.clj:76)

    merge-treesok units-test/merge-test (units_test.clj:77)

  find-valuesok units-test/find-values (units_test.clj:101)

    find-item-from-tree-existingok units-test/find-values (units_test.clj:105)

    find-item-from-tree-no-existing
  insertok units-test/insert (units_test.clj:26)

    inserting-item-in-empty-treeok units-test/insert (units_test.clj:27)
ok units-test/insert (units_test.clj:34)

    inserting-item-in-tree-with-valid-check-int-keys-int-valuesok units-test/insert (units_test.clj:35)
ok units-test/insert (units_test.clj:42)

    inserting-item-in-tree-with-balance-check-string-keys-int-valuesok units-test/insert (units_test.clj:43)
ok units-test/insert (units_test.clj:50)

    inserting-item-in-tree-with-balance-check-string-keys-string-valuesok units-test/insert (units_test.clj:51)
ok units-test/insert (units_test.clj:58)

    inserting-item-in-tree-with-balance-check-int-keys-string-valuesok units-test/insert (units_test.clj:59)
ok units-test/insert (units_test.clj:66)

    inserting-items-in-treeok units-test/insert (units_test.clj:67)

  containok units-test/contain (units_test.clj:14)

    containsok units-test/contain (units_test.clj:17)

    not-contains

property-test
  insert-elemok property-test/insert-elem (assertions.cljc:26)

  empty-monoidok property-test/empty-monoid (assertions.cljc:26)

  contains-all-elemsok property-test/contains-all-elems (assertions.cljc:26)

  merge-containsok property-test/merge-contains (assertions.cljc:26)

  fold-clearok property-test/fold-clear (assertions.cljc:26)

  insert-delete-equalsok property-test/insert-delete-equals (assertions.cljc:26)

  contains-one-random-elemok property-test/contains-one-random-elem (assertions.cljc:26)

  filter-elemok property-test/filter-elem (assertions.cljc:26)

  merge-monoidok property-test/merge-monoid (assertions.cljc:26)

  delete-elemok property-test/delete-elem (assertions.cljc:26)

1..49

19 tests, 49 assertions, 0 failures.
```

### Заключение

Выполняя данную лабораторную работу, я реализовал словарь через ***AVL*** дерево, познакомился с ***property-based*** тестированием, написал пару тестов, помучился с генераторами, потому что простые генераторы для типа ***Number*** при небольшом кол-ве генерирумых чисел все время генерировали одинаковые числа, поэтому пришлось искать выход из ситуации и читать много документации. К сожалению, там мало описано примеров, поэтому пришлось все самому пробовать. 