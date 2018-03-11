(ns exercises.math)

(defn !
  "Returns <n> factorial (!n)."
  [n]
  {:pre [(not (neg? n))]}
  (if (zero? n)
    1
    (reduce *' (range 1 (inc n)))))

;; kudos to Christophe Grand for this:
;; http://clj-me.cgrand.net/index.php?s=Primes
(defn primes
  "A (fast) prime generator that won't blow your stack."
  ([n]
   (take n (primes)))
  ([]
   (letfn [(enqueue [sieve n step]
             (let [m (+ n step)]
               (if (sieve m)
                 (recur sieve m step)
                 (assoc sieve m step))))
           (next-sieve [sieve candidate]
             (if-let [step (sieve candidate)]
               (-> sieve
                   (dissoc candidate)
                   (enqueue candidate step))
               (enqueue sieve candidate (+ candidate candidate))))
           (next-primes [sieve candidate]
             (if (sieve candidate)
               (recur (next-sieve sieve candidate) (+ candidate 2))
               (cons candidate
                     (lazy-seq (next-primes (next-sieve sieve candidate)
                                            (+ candidate 2))))))]
     (cons 2 (lazy-seq (next-primes {} 3))))))

(defn prime?
  "Optimized primality test per:
   https://en.wikipedia.org/wiki/Primality_test"
  [x]
  (condp >= x
    ; be clever
    1 false ;; x ≤ 1
    3 true  ;; x ≤ 3
    ;; else
    (let [lud (Math/sqrt (double x))] ;; largest-useful-divisor
      (->> (primes)
           (take-while (partial >= lud))
           (cons 2)
           (not-any? (comp zero? (partial rem x)))))))

(defn fibonacci
  "My favourite way of calculating the fibonacci sequence.
   No need to create laziness manually - use `iterate`."
  [n]
  (->> [0 1]    ;; start with the first two natural numbers as the recursive base case
       (iterate ;; use them as the seed for a sequence of tuples
         (fn [[a b]]
           ;; where the last element of each tuple (the current calculated fib)
           ;; appears as the first element of the next tuple
           [b (+ a b)]))
       (map first)
       (take n))) ;; finally extract the n first elements


(defn fizz-buzz
  "The infamous fizz-buzz game as a lazy seq."
  [n]
  (map
    (fn [i]
      (or
        (cond-> nil
                (zero? (rem i 3)) (str "Fizz")
                (zero? (rem i 5)) (str "Buzz"))
        (str i)))
    (range 1 (inc n))))


;; CHURCH NUMERALS

(defn chumeral ;; church-numeral
  "Returns a function that maps <f> to its n-fold composition (Church encoding).
   Natural numbers (non-negative integers) can be represented this way."
  [n]
  (assert (not (neg? n)))
  ;; `comp` with no args returns `identity`,
  ;; so no need to special-case i=0
  (fn [f]
    (reduce comp (repeat n f))))


(defn unchumeral
  "The opposite of `chumeral`."
  [chumeral]
  ((chumeral inc) 0))


(defn chu+
  [cnx]
  (fn [cny]
    (fn [f]
      (fn [x]
        ((cnx f)
          ((cny f) x))))))

(defn chu-inc
  [cnx]
  (fn [f]
    (fn [x]
      (((chumeral 1) f) ;; ((chumeral 1) f)  => f
        ((cnx f) x)))))


(defn chu-pred
  [cnx]
  (fn [f]
    (fn [x]
      (((cnx (fn [g]
               (fn [h]
                 (h (g f)))))
         (constantly x))
        (fn [u] u)))))

(defn chu-minus
  [cnx]
  (fn [n]
    ((n chu-pred) cnx)))


(defn chu-mult
  [cnx]
  (fn [cny]
    (fn [f]
      (cny
        (cnx f)))))


(defn chu-exp
  [cnx]
  (fn [cny]
    (cny cnx)))

(defn chu-zero?
  [cnx]
  ((cnx (constantly false)) true))


(defn- Y-recur
  [yrecur]
  (fn [y]
    ((yrecur yrecur) y)))

(defn Y
  "The infamous Y-combinator"
  [f]
  ((fn [x]
     (x x))
    (fn [x]
      (f (Y-recur x)))))