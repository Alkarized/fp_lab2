(ns units-test
  (:require [clojure.test :refer [deftest testing is run-tests]]
            ;;[AVL-dict :as avl]
            ))

(deftest testings
  (testing "ez_way"
    (is (= 1 1))))

(run-tests)