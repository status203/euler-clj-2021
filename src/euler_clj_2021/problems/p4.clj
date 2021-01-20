(ns euler-clj-2021.problems.p4
  (:require [euler-clj-2021.math.numbers :refer [palindromic?]]))

;; A palindromic number reads the same both ways. The largest palindrome made 
;; from the product of two 2-digit numbers is 9009 = 91 × 99.

;; Find the largest palindrome made from the product of two 3-digit numbers.

(->> (for [x (range 100 999)
           y (range (inc x) 1000)]
       (* x y))
     (filter palindromic?)
     (apply max))