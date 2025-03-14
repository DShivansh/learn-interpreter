(ns learn-interpreter.evaluator_test
  (:require [clojure.test :refer :all]
            [learn-interpreter.evaluator :refer :all :as evaluator]))

(defn testing-helper [expression expected-output]
  (let [output (evaluator/evaluation expression)]
    (= (.Value output) expected-output)))

(deftest test-integer-expression
  (testing "Testing expression for 5"
    (is (testing-helper "5;" 5)))
  (testing "Testing expression for 10"
    (is (testing-helper "10;" 10))))

(deftest test-boolean-expression
  (testing "Testing expression for true"
    (is (testing-helper "true;" true)))
  (testing "Testing expression for false"
    (is (testing-helper "false;" false))))

(deftest test-bang-operator
  (testing "Testing on true"
    (is (testing-helper "!true;" false)))
  (testing "Testing on false"
    (is (testing-helper "!false;" true)))
  (testing "Testing on number"
    (is (testing-helper "!5;" false)))
  (testing "Testing double on true"
    (is (testing-helper "!!true;" true)))
  (testing "Testing double on false"
    (is (testing-helper "!!false;" false)))
  (testing "Testing double on number"
    (is (testing-helper "!!5;" true))))

