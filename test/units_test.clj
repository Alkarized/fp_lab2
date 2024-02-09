(ns units-test
  (:require [clojure.test :refer [deftest testing is run-tests]]
            [AVL-dict :as avl]))

(defn is-balanced? [tree]
  (if (nil? tree) true
      (let [left-h (avl/height (:left tree))
            right-h (avl/height (:right tree))]
        (and (<= (Math/abs (- left-h right-h)) 1)
             (is-balanced? (:left tree))
             (is-balanced? (:right tree))))))

(defn binary-helper [node min-value max-value] ;; Работает только с целыми значениями в key
  (if (nil? node) true
      (and (and (<= (compare min-value (:key node)) 0) (>= (compare max-value (:key node)) 0))
           (binary-helper (:left node) min-value (:key node))
           (binary-helper (:right node) (:key node) max-value))))

(defn is-binary? [tree]
  (binary-helper tree Long/MIN_VALUE Long/MAX_VALUE))

(defn is-valid-tree? [tree]
  (and (is-binary? tree) (is-balanced? tree)))

;; Вспомогательная функция для проверки, что в дереве есть все элементы из данного списка
(defn check-itmes [tree seq]
  (every? (fn [[k v]] (= v (avl/get-value tree k))) seq))

(def keys-seq [123 124 634 745 3 15 75 61])
(def values-seq [451 61 152 23 5 612 12 64])
(def seq1 [[1522 1] [2 54] [651 325] [128 123] [9874 123]])
(def seq2 [[512 234] [543 1231] [645 42316] [561 785] [126 125]])

(deftest contain
  (testing "contains"
    (let [tree (avl/to-tree [[412 123]])]
      (is (avl/contains tree 412))))
  (testing "not-contains"
    (let [tree (avl/to-tree [[412 123]])]
      (is (not (avl/contains tree 1))))))

(deftest insert
  (testing "inserting-item-in-empty-tree"
    (let [tree-nil nil
          expected-tree (avl/node 4 2 nil nil)
          new-tree (avl/insert tree-nil 4 2)]

      (is (= expected-tree new-tree))
      (is (is-valid-tree? new-tree))))

  (testing "inserting-item-in-tree-with-valid-check-int-keys-int-values"
    (let [tree-base (avl/to-tree (map vector keys-seq values-seq))
          tree-expected (avl/to-tree (map vector (conj keys-seq 73) (conj values-seq 41)))
          tree-added (avl/insert tree-base 73 41)]

      (is (is-valid-tree? tree-added))
      (is (= tree-added tree-expected))))

  (testing "inserting-item-in-tree-with-balance-check-string-keys-int-values"
    (let [tree-base (avl/to-tree (map vector (map str keys-seq) values-seq))
          tree-expected (avl/to-tree (map vector (map str (conj keys-seq 73)) (conj values-seq 41)))
          tree-added (avl/insert tree-base "73" 41)]

      (is (is-balanced? tree-added))
      (is (= tree-added tree-expected))))

  (testing "inserting-item-in-tree-with-balance-check-string-keys-string-values"
    (let [tree-base (avl/to-tree (map vector (map str keys-seq) (map str values-seq)))
          tree-expected (avl/to-tree (map vector (map str (conj keys-seq 73)) (map str (conj values-seq 41))))
          tree-added (avl/insert tree-base "73" "41")]

      (is (is-balanced? tree-added))
      (is (= tree-added tree-expected))))

  (testing "inserting-item-in-tree-with-balance-check-int-keys-string-values"
    (let [tree-base (avl/to-tree (map vector keys-seq (map str values-seq)))
          tree-expected (avl/to-tree (map vector (conj keys-seq 73) (map str (conj values-seq 41))))
          tree-added (avl/insert tree-base 73 "41")]

      (is (is-valid-tree? tree-added))
      (is (= tree-added tree-expected))))

  (testing "inserting-items-in-tree"
    (let [tree-base  (avl/to-tree seq1)
          tree-expected (avl/to-tree (concat seq1 seq2))
          tree-actual (avl/insert-sequence tree-base seq2)]

      (is (avl/equal-trees? tree-actual tree-expected))
      (is (is-valid-tree? tree-actual)))))

(deftest merge-test
  (testing "merge-trees"
    (let [tree1 (avl/to-tree seq1)
          tree2 (avl/to-tree seq2)
          tree-actual (avl/merge-insert tree1 tree2)]

      (is (check-itmes tree-actual (concat seq1 seq2)))
      (is (is-valid-tree? tree-actual)))))

(deftest delete
  (testing "delete-item-from-tree-with-balance-check"
    (let [tree-base (avl/to-tree seq1)
          tree-with-deleted (avl/remove-node tree-base 9874)]

      (is (avl/contains tree-base 9874))
      (is (not (avl/contains tree-with-deleted 9874)))
      (is (is-valid-tree? tree-with-deleted))))

  (testing "delete-item-from-tree-with-balance-check-where-no-this-item"
    (let [tree-base (avl/to-tree seq1)
          tree-with-deleted (avl/remove-node tree-base 1)]

      (is (not (avl/contains tree-base 1)))
      (is (not (avl/contains tree-with-deleted 1)))
      (is (is-valid-tree? tree-with-deleted)))))

(deftest find-values
  (testing "find-item-from-tree-existing"
    (let [tree-base (avl/to-tree seq2)]
      (is (= (avl/get-value tree-base 645) 42316))))

  (testing "find-item-from-tree-no-existing"
    (let [tree-base (avl/to-tree seq2)]
      (is (= (avl/get-value tree-base 564) nil)))))

(deftest folds
  (testing "fold-left-sum-values"
    (let [tree-base (avl/to-tree seq1)
          sum1      (avl/fold-left tree-base (fn [acc k v] (+ acc v (* k 0))) 0)
          sum2      (reduce (fn [acc [k v]] (+ acc v (* 0 k))) 0 seq1)]
      (is (= sum1 sum2))))

  (testing "fold-right-sum-keys"
    (let [tree-base (avl/to-tree seq1)
          sum1 (avl/fold-right tree-base (fn [acc k v] (+ acc k (* v 0))) 0)
          sum2 (reduce (fn [acc [k v]] (+ acc k (* 0 v))) 0 seq1)]
      (is (= sum1 sum2)))))

(deftest filter-avl
  (testing "filter-by-even-nums-in-values"
    (let [tree-base (avl/to-tree seq1)
          filtered-tree-actual (avl/filter-tree tree-base (fn [k v] (even? (+ v (* 0 k)))))
          filtered-tree-expected (avl/to-tree (filter (fn [[k v]] (even? (+ v (* 0 k)))) seq1))]

      (is (= filtered-tree-actual filtered-tree-expected))
      (is (is-valid-tree? filtered-tree-actual))))

  (testing "filter-by-odd-nums-in-keys"
    (let [tree-base (avl/to-tree seq2)
          filtered-tree-actual (avl/filter-tree tree-base (fn [k v] (odd? (+ k (* 0 v)))))
          filtered-tree-expected (avl/to-tree (filter (fn [[k v]] (odd? (+ k (* 0 v)))) seq2))]

      (is (= filtered-tree-actual filtered-tree-expected))
      (is (is-valid-tree? filtered-tree-actual)))))

(deftest map-avl
  (testing "apply-x2-to-values"
    (let [tree-base            (avl/to-tree seq1)
          mapped-tree-actual   (avl/map-tree tree-base (fn [k v] {:key   k
                                                                  :value (* 2 v)}))
          mapped-tree-expected (avl/to-tree (mapv (fn [[k v]] (vector k (* 2 v))) seq1))
          sum1                 (avl/fold-left tree-base (fn [acc k v] (+ acc v (* k 0))) 0)
          sum2                 (avl/fold-left mapped-tree-actual (fn [acc k v] (+ acc v (* k 0))) 0)]
      (is (= mapped-tree-actual mapped-tree-expected))
      (is (is-valid-tree? mapped-tree-actual))
      (is (= (* sum1 2) sum2))))

  (testing "apply-x10-to-keys"
    (let [tree-base            (avl/to-tree seq1)
          mapped-tree-actual   (avl/map-tree tree-base (fn [k v] {:key   (* 10 k)
                                                                  :value v}))
          mapped-tree-expected (avl/to-tree (mapv (fn [[k v]] (vector (* 10 k) v)) seq1))
          sum1                 (avl/fold-left tree-base (fn [acc k v] (+ acc k (* v 0))) 0)
          sum2                 (avl/fold-left mapped-tree-actual (fn [acc k v] (+ acc k (* v 0))) 0)]
      (is (= mapped-tree-actual mapped-tree-expected))
      (is (is-valid-tree? mapped-tree-actual))
      (is (= (* sum1 10) sum2)))))

(deftest monoid
  (testing "check-empty-tree"
    (let [tree-non-empty (avl/to-tree seq1)
          empty-tree     nil
          merged-left    (avl/merge-insert empty-tree tree-non-empty)
          merged-right   (avl/merge-insert tree-non-empty empty-tree)]
      (is (= merged-left merged-left tree-non-empty))
      (is (is-valid-tree? merged-left))
      (is (is-valid-tree? merged-right)))))

(run-tests)