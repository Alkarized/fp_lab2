(ns property-test
  (:require [clojure.test :refer [deftest testing is run-tests]]
            [AVL-dict :as avl]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]))

(def seq1 [[1 2] [3 1] [61 3]])
(def tree (avl/to-tree seq1))

(deftest testings
  (testing "ez_way"
    (is (= 1 1))))

(defspec sort-is-idempotent 100
  (prop/for-all [v (gen/vector gen/small-integer)]
                (= (sort v) (sort (sort v)))))

(run-tests)