(ns io.github.frenchy64.fully-satisfies.expand-kvs-test
  (:require [clojure.test :refer [is are]]
            [io.github.frenchy64.fully-satisfies.uncaught-testing-contexts
             :refer [deftest testing]]
            [io.github.frenchy64.fully-satisfies.expand-kvs :refer [flatten-trailing-map]]))

(deftest flatten-trailing-map-test
  (is (= []
         (flatten-trailing-map
           2
           (list))))
  (is (= []
         (flatten-trailing-map
           (list))
         (flatten-trailing-map
           0
           (list))))
  (is (= [1 2 :a 1]
         (flatten-trailing-map
           2
           (list 1 2 (sorted-map :a 1)))))
  (is (= [:a 1]
         (flatten-trailing-map
           (list (sorted-map :a 1)))
         (flatten-trailing-map
           0
           (list (sorted-map :a 1)))))
  (is (= [:a 1 :b 2 :c 3]
         (flatten-trailing-map
           (list (sorted-map :a 1 :b 2 :c 3)))
         (flatten-trailing-map
           0
           (list (sorted-map :a 1 :b 2 :c 3)))))
  (is (= []
         (flatten-trailing-map
           (list (sorted-map)))
         (flatten-trailing-map
           0
           (list (sorted-map)))))
  (is (= [:a 1 :b 2]
         (flatten-trailing-map
           (list :a 1 (sorted-map :b 2)))
         (flatten-trailing-map
           0
           (list :a 1 (sorted-map :b 2)))))
  (is (= [:a 1]
         (flatten-trailing-map
           (list :a 1 (sorted-map)))
         (flatten-trailing-map
           0
           (list :a 1 (sorted-map)))))
  (is (= [1 2 3 :a 1 :b 2]
         (flatten-trailing-map
           3
           (list 1 2 3 :a 1 (sorted-map :b 2)))))
  (is (= [:uneven]
         (flatten-trailing-map
           (list :uneven))
         (flatten-trailing-map
           0
           (list :uneven))))
  (is (= [:less :than :four]
         (flatten-trailing-map
           4
           (list :less :than :four)))))
