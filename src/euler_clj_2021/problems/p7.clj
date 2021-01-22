(ns euler-clj-2021.problems.p7
  (:require [euler-clj-2021.math.numbers :refer [primes]]))

;; By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that 
;; the 6th prime is 13.

;; What is the 10 001st prime number?

(->> (primes)
     (drop 10000)
     (first))

