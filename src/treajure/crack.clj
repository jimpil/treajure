(ns treajure.crack
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :as combi]
            [treajure.trance :as trance])
  (:import (java.util.concurrent.atomic AtomicLong)))


(defn- search-space
  [space try-it answer-promise end?]
  (future
    (if-let [found (trance/some-interruptible
                     (map try-it)
                     space)]
      (deliver answer-promise found)
      (when (end?)
        (deliver answer-promise nil)))))

(defn brute-force
  ""
  [try-selection allowed lengths]
  (assert (every? integer? lengths))
  (let [answer (promise)
        end-counter (AtomicLong. (count lengths))
        end? #(zero? (.decrementAndGet end-counter))
        futures (mapv #(search-space (combi/selections allowed %) try-selection answer end?) lengths)]
    (when-let [res @answer]
      (run! future-cancel futures)
      res)))


(comment
  (let [test-pwd (seq "1943")]
    (brute-force #(when (= test-pwd %)
                    (apply str %))
                 (set (map char (range 48 58)))
                 ;(set (map char (range (int \a) (inc (int \z)))))
                 [3 4 5 6]))



  )


(defn dict-attack [try-fn ^String dict-path line-count]
  (let [cpus (.. Runtime getRuntime availableProcessors)
        normal-partition (quot line-count cpus)
        splits* (take (dec cpus)
                      (iterate (fn [[pstart pend]]
                                 [pend (+ pend normal-partition)])
                               [0 normal-partition]))
        r* (rem line-count cpus)
        last-partition (let [pend (-> splits* last second)]
                         [pend (cond-> (+ pend normal-partition)
                                       (not (zero? r*)) (+ r*))])
        splits (-> splits* vec (conj last-partition))

        end-counter (AtomicLong. (count splits))
        end? #(zero? (.decrementAndGet end-counter))
        answer (promise)
        futures (mapv (fn [[from to]]
                        (search-space (trance/lines-reducible-between (io/reader dict-path) from to)
                                      try-fn
                                      answer
                                      end?))
                      splits)]
    (when-let [res @answer]
      (run! future-cancel futures)
      res)))

(comment
  (dict-attack
    #(when (= "1943" %) %)
    "/home/dimitris/Desktop/test-dict.txt"))