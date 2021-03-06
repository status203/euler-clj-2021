(ns euler-clj-2021.problems.p3
  (:require [euler-clj-2021.math.numbers :refer [prime-factors]]))

;; The prime factors of 13195 are 5, 7, 13 and 29.

;; What is the largest prime factor of the number 600851475143 ?

(reduce max (prime-factors 600851475143))
;; => 6857
