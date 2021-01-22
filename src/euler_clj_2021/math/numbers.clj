(ns euler-clj-2021.math.numbers)

(defn multiple?
  "Returns whether n is a multiple of base"
  [base n]
  (zero? (mod n base)))

(defn divides?
  "Returns whether base divides n"
  [n base]
  (zero? (mod n base)))

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
       (filter #(multiple? % n))))

(defn prime?
  "(First prime test) niave check  - returns whether n is prime"
  [n]
  (cond
    (< n 2) false
    (= n 2) true
    :else (let [upper-bound (Math/floor (Math/sqrt n))
                potential-factors (cons 2 (range 3 upper-bound 2))] 
            (->> potential-factors
                 (filter #(multiple? % n))
                 empty?))))

(defn next-prime
  "Returns the next prime after the supplied sequence of primes and the highest
   of that sequence (to avoid 'last' on a seq). Assumes highest-prev-prime is at
   least 3"
  [prev-primes highest-prev-prime]
  (loop [candidates (iterate (partial +' 2) (+' 2 highest-prev-prime))]
    (let [candidate (first candidates)
          upper-bound (Math/floor (Math/sqrt candidate))
          potential-factors (take-while (partial <= upper-bound) prev-primes)]
      (if (not-any? (partial divides? candidate) prev-primes)
        candidate
        (recur (rest candidates))))))

(defn rest-primes
  "Returns a lazy sequence of the subsequent primes to the supplied sequence and
   highest value of that sequence. Assumes highest-prev-prime is at least 3"
  [prev-primes highest-prev-prime]
  (let [next-prime-value (next-prime prev-primes highest-prev-prime)
        next-previous-primes (concat prev-primes [next-prime-value])]
    (lazy-seq (cons next-prime-value (rest-primes next-previous-primes
                                                  next-prime-value)))))

(defn primes
  "Returns a lazy infinite sequence of primes"
  []
  (concat [2 3 5 7]
          (rest-primes [2 3 5 7] 7)))

(defn prime-factors
  "Returns a complete list of prime factors of n. E.g. the prime factors of 12 are
   [2 2 3]. If supplied with a sequence of primes it uses those (avoids calling
   (primes) every time if you're factoring a lot))"
  ([n]
   (prime-factors n (primes)))
  ([n potential-factors]
   (loop [potential-factors potential-factors
          remaining n
          acc-factors []]
     (if (= remaining 1)
       acc-factors ; base case
       (let [potential-factor (first potential-factors)]
         (if (multiple? potential-factor remaining)
           (recur potential-factors
                  (quot remaining potential-factor) ; reduce remaining
                  (conj acc-factors potential-factor)) 
           (recur (rest potential-factors) remaining acc-factors))))))) ; remove non-factor

(defn square [n] (* n n))

(defn palindromic?
  "Returns whether n is palindromic"
  [n]
  (let [str-n (str n)]
    (= str-n (apply str (reverse str-n)))))

(defn expand-frequencies
  "Takes a map of frequencies and returns a sequence of the specified number of
  eack value. E.g. {2 3 5 1} returns (2 2 2 5)"
  [m]
  (reduce (fn [acc [k v]] (concat (repeat v k) acc))
          []
          m))

(defn least-common-multiple
  "Returns the least common multiple of a sequence of numbers"
  ([] 1)
  ([n] n)
  ([n & rst]
   (->> (cons n rst)
        (map prime-factors)
        (map frequencies)
        (reduce (partial merge-with max))
        expand-frequencies
        (apply *))))