(ns euler-clj-2021.problems.p10
  (:require [euler-clj-2021.math.numbers :refer [primes]]))

;; The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

;; Find the sum of all the primes below two million.

(->> (primes)
     (take-while (partial > 2000000))
     (apply +'))
;; => 142913828922


