(ns euler-clj-2021.problems.p9
  (:require [euler-clj-2021.math.numbers :refer [square]]))

(->>  (for [a (range  1 (inc 332))
            b (range  a (inc (quot (- 1000 a) 2)))]
        [a b (- 1000 a b)])
      (filter (fn [[a b c]] (= (+ (square a) (square b)) (square c))))
      first
      (apply *))
;; => 31875000

