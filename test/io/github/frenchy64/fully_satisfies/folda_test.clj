(ns io.github.frenchy64.fully-satisfies.somef-test
  (:require [clojure.test :refer :all]
            [io.github.frenchy64.fully-satisfies.folda :refer [folda]]))

(deftest folda-test
  (testing "without aname"
    (is (= [1 2 3]
           (let [a (int-array [1 2 3])]
             (folda a
                    i
                    acc []
                    (conj acc (aget a i)))))))
  (testing "with aname"
    (is (= [1 2 3]
           (folda a (int-array [1 2 3])
                  i
                  acc []
                  (conj acc (aget a i))))))
  (testing "hygiene"
    (testing "aname shadows idx, but only in expr"
      (let [the-array (int-array [1 2 3])]
        (is (= (repeat 3 the-array)
               (folda a the-array
                      a
                      acc []
                      (conj acc a))))))
    (testing "aname shadows ret, but only in expr"
      (is = [1 2 3]
          (let [res (atom [])]
            (folda a (int-array [1 2 3])
                   i
                   a []
                   (swap! res conj (aget a i))))))))
