(ns euler-clj-2021.problems.p3
  (:require [euler-clj-2021.math.numbers :refer [distinct-prime-factors]]))

;; The prime factors of 13195 are 5, 7, 13 and 29.

;; What is the largest prime factor of the number 600851475143 ?

(reduce max (distinct-prime-factors 13195))