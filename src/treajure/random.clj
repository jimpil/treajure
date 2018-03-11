(ns treajure.random
  (:import (java.util Random)
           (java.security SecureRandom)))

(defn uni-random-producer
  "Uniformly distributed random numbers based on `java.util.Random`.
   Returns a function with 2 arities. The 0-arg one will produce elements indefinitely,
   whereas the 1-arg arity will generate a lazy-seq of <n> elements
   (where <n> is the argument expected - an integer)."
  ([]
   (uni-random-producer nil))
  ([seed]
   (uni-random-producer seed :long))
  ([seed prod-type]
   (uni-random-producer seed prod-type nil))
  ([seed prod-type bound]
   (let [rnd (if seed
               (Random. seed)
               (Random.))
         f (case prod-type
             :boolean #(.nextBoolean rnd)
             :gaussian #(.nextGaussian rnd)
             :double #(.nextDouble rnd)
             :long #(.nextLong rnd)
             :int (if bound
                    #(.nextInt rnd)
                    #(.nextInt rnd bound)))]
     (fn
       ([]
        (repeatedly f))
       ([n]
        (repeatedly n f))))))

(defn secure-random-bytes
  "Returns a random byte array of the specified size."
  ^bytes [size]
  (let [ret (byte-array size)]
    (.nextBytes (SecureRandom.) ret)
    ret))
