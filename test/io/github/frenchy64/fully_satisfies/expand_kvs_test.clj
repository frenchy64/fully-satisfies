(ns io.github.frenchy64.fully-satisfies.expand-kvs-test
  (:require [clojure.test :refer [is are]]
            [io.github.frenchy64.fully-satisfies.uncaught-testing-contexts
             :refer [deftest testing]]
            [io.github.frenchy64.fully-satisfies.expand-kvs :refer [expand-kvs-seq]]))

(deftest expand-kvs-seq-test
  (is (= []
         (expand-kvs-seq
           2
           (list))))
  (is (= []
         (expand-kvs-seq
           0
           (list))))
  (is (= [1 2 :a 1]
         (expand-kvs-seq
           2
           (list 1 2 (sorted-map :a 1)))))
  (is (= [:a 1]
         (expand-kvs-seq
           0
           (list (sorted-map :a 1)))))
  (is (= [:a 1 :b 2 :c 3]
         (expand-kvs-seq
           0
           (list (sorted-map :a 1 :b 2 :c 3)))))
  (is (= []
         (expand-kvs-seq
           0
           (list (sorted-map)))))
  (is (= [:a 1 :b 2]
         (expand-kvs-seq
           0
           (list :a 1 (sorted-map :b 2)))))
  (is (= [:a 1]
         (expand-kvs-seq
           0
           (list :a 1 (sorted-map)))))
  (is (= [1 2 3 :a 1 :b 2]
         (expand-kvs-seq
           3
           (list 1 2 3 :a 1 (sorted-map :b 2)))))
  (is (= [:uneven]
         (expand-kvs-seq
           0
           (list :uneven))))
  (is (= [:less :than :four]
         (expand-kvs-seq
           4
           (list :less :than :four)))))
