(ns euler-clj-2021.math.numbers)

(defn multiple?
  "Returns whether x is a multiple of base"
  [num x]
  (= 0 (mod num x)))

(defn fibonacci
  "Creates an infinite lazy fibonacci sequence starting with m, n (0,1 by default)"
  ([] (fibonacci 0 1))
  ([m n] (lazy-seq
          (cons m
                (fibonacci n (+ m n))))))

(defn factors
  "Returns a sequence of all factors of n (incl. 1 and n)"
  [n]
  (->> (range 1 (inc n))
       (filter #(multiple? n %))))

(defn prime?
  "(First prime test) niave check  - returns whether n is prime"
  [n]
  (cond 
    (< n 2) false
    (= n 2) true
    :else (->> (range 2 n)
               (filter #(multiple? n %))
               empty?)))
(defn primes
  []
  (concat [2 3 5 7 11 13]
          (->> (iterate (partial + 2) 15)
               (filter prime?))))

(defn square [n] (* n n))

(defn distinct-prime-factors
  "Returns a list of prime factors of n. n must be a positive integer > 1"
  [n]
  (->> (take-while #(<= % n) (primes))
       (reduce (fn [acc prime]
                 (if (multiple? n prime)
                   (conj acc prime)
                   acc))
               [])))

