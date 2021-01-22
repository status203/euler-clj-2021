(ns euler-clj-2021.problems.p5
  (:require [euler-clj-2021.math.numbers :refer [least-common-multiple]]))

;; 2520 is the smallest number that can be divided by each of the numbers from 
;; 1 to 10 without any remainder.

;; What is the smallest positive number that is evenly divisible by all of the 
;; numbers from 1 to 20?

(apply least-common-multiple (range 2 21))
;; => 232792560

