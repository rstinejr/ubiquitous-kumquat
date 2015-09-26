(ns variance.core-test
  (:require [clojure.test  :refer :all]
            [variance.core :refer :all]))

(def easy-sample [2 3 4])
(def easy-expect (double (/ 2.0 3.0)))

(def big-sample [0 1 2 3 4 5 6])
(def big-expect (double (/ 28.0 7.0)))

(defn check-calc
  [f sample expect]
  (let [v    (f sample)
        err  (- expect v)
        pct  (/ err expect)
        pct2 (* pct pct)]
    (> 0.00001 pct2)))

(defn run-easy
  [f]
  (check-calc f easy-sample easy-expect))

(defn run-bigger
  [f]
  (check-calc f big-sample big-expect))

(deftest test-calculate-variance1
  (testing "easy case"
    (is (run-easy calculate-variance1)))
  (testing "bigger variance"
    (is (run-bigger calculate-variance1)))
  (testing "no samples"
    (is (thrown? Exception (calculate-variance1 [])))))

(deftest test-calculate-variance2
  (testing "easy case"
    (is (run-easy calculate-variance2)))
  (testing "bigger variance"
    (is (run-bigger calculate-variance2)))
  (testing "no samples"
    (is (thrown? Exception (calculate-variance2 [])))))

(deftest test-calculate-variance3
  (testing "easy case"
    (is (run-easy calculate-variance3)))
  (testing "bigger variance"
    (is (run-bigger calculate-variance3)))
  (testing "no samples"
    (is (thrown? Exception (calculate-variance3 [])))))
