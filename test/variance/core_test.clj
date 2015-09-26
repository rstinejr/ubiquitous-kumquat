(ns variance.core-test
  (:require [clojure.test  :refer :all]
            [variance.core :refer :all]))

(def easy-sample [2 3 4])
(def easy-expect (double (/ 2.0 3.0)))

(deftest test-calculate-variance1
  (testing "easy case"
    (let [v      (calculate-variance1 easy-sample)]
      (let [err   (- easy-expect v)
            pct   (/ err easy-expect)
            pct2  (* pct pct)]
        (is (> 0.00001 pct2)))))
  (testing "bigger variance"
    (let [sample [0 1 2 3 4 5 6]
          v      (calculate-variance1 sample)]
      (let [exp   (double (/ 28.0 7.0))
            err   (- exp v)
            pct   (/ err exp)
            pct2  (* pct pct)]
        (is (> 0.00001 pct2)))))
  (testing "no samples"
    (is (thrown? Exception (calculate-variance1 [])))))

(deftest test-calculate-variance2
  (testing "easy case"
    (let [v      (calculate-variance2 easy-sample)]
      (let [err   (- easy-expect v)
            pct   (/ err easy-expect)
            pct2  (* pct pct)]
        (is (> 0.00001 pct2)))))
  (testing "bigger variance"
    (let [sample [0 1 2 3 4 5 6]
          v      (calculate-variance2 sample)]
      (let [exp   (double (/ 28.0 7.0))
            err   (- exp v)
            pct   (/ err exp)
            pct2  (* pct pct)]
        (is (> 0.00001 pct2)))))
  (testing "no samples"
    (is (thrown? Exception (calculate-variance2 [])))))
