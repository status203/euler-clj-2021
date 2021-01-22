(ns euler-clj-2021.problems.p6
  (:require [euler-clj-2021.math.numbers :refer [square]]))

;; The sum of the squares of the first ten natural numbers is 385

;; The square of the sum of the first ten natural numbers is 3025

;; The difference between the sum of the squares of the first ten natural 
;; numbers and the square of the sum is 2640.

;; Find the difference between the sum of the squares of the first one hundred 
;; natural numbers and the square of the sum.

(-
 (->> (range 101)
      (apply +')
      (square))
 (->> (range 101)
      (map square)
      (apply +')))
;; => 25164150

