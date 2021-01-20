(ns euler-clj-2021.math.numbers-test
  (:require [clojure.test :refer [deftest testing is are]]
            [euler-clj-2021.math.numbers :as num]))

(deftest divides-tests
  (testing "divides:"
    (testing "num is a multiple of x"
     (are [num x] (num/multiple? num x)
       1 1
       2 1
       2 2
       12 1
       12 2
       12 3
       12 4
       12 6
       12 12))
    (testing "num is not a multipel of x"
      (are [num x] (not (num/multiple? num x))
        1 2
        5 3))))

(deftest prime?-tests
  (testing "prime?:"
    (testing "n is prime"
      (are [n] (num/prime? n)
        2 3 5 67))
    (testing "n is not prime"
      (are [n] (not (num/prime? n))
        -5 -1 0 1 4 62))))

(deftest next-prime-tests
  (testing "next-prime:"
    (are [prev-primes highest-prev-prime expected]
         (= expected (num/next-prime prev-primes highest-prev-prime))
      [2 3] 3 5
      [2 3 5 7] 7 11
      [2 3 5 7 11 13] 13 17)))

(deftest rest-primes-tests
  (testing "rest-primes:"
    (are [prev-primes highest-prev-prime expected] 
         (= expected (take 5 (num/rest-primes prev-primes highest-prev-prime)))
      [2 3] 3 [5 7 11 13 17]
      [2 3 5 7 11] 11 [13 17 19 23 29])))

(deftest primes-tests
  (testing "primes:"
    (is (= [2 3 5 7 11 13 17 19 23 29] (take 10 (num/primes))))))

(deftest prime-factors-tests
    (testing "prime-factors-tests:"
      (are [n expected] (= expected (num/prime-factors n))
        2 [2]
        6 [2 3]
        8 [2 2 2]
        12 [2 2 3]
        13 [13])))

(deftest factors-tests
  (testing "divisors:"
    (are [n expected] (= expected (num/factors n))
      -1 []
      0 []
      1 [1]
      2 [1 2]
      6 [1 2 3 6]
      8 [1 2 4 8])))

(deftest fibonacci-tests
  (testing "fibonacci:"
    (testing "first 5entries of default call"
      (is (= [0, 1, 1, 2, 3] (take 5 (num/fibonacci)))))

    (testing "first five entries of specified sequences"
      (are [m n expected] (= expected (take 5 (num/fibonacci m n)))
        0 0 [0 0 0 0 0]
        1 1 [1 1 2 3 5]
        5 6 [5 6 11 17 28]))))

(deftest palindromic?
  (testing "palindromic?:"
    (testing "should be true"
      (are [n] (true? (num/palindromic? n))
        0
        1
        55
        135531))
    (testing "should be false"
      (are [n] (false? (num/palindromic? n))
        10
        1355311))))

#_(deftest proper-factors-tests
  (testing "proper-divisors:"
    (are [n expected] (= expected (num/proper-factors n))
      -1 []
      0 []
      1 []
      2 [1]
      6 [1 2 3]
      8 [1 2 4])))