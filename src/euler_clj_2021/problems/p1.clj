(ns euler-clj-2021.problems.p1)


;; If we list all the natural numbers below 10 that are multiples of 3 or 5, we 
;; get 3, 5, 6 and 9. The sum of these multiples is 23.

;; Find the sum of all the multiples of 3 or 5 below 1000.

(->> 1000
     (range 1)
     (filter #(or (= 0 (mod % 3))
                  (= 0 (mod % 5))))
     (reduce +))