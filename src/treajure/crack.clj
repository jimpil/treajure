(ns treajure.crack
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :as combi]
            [treajure.trance :as trance]
            [treajure.jlamda :as jl]
            [treajure.jstreams :as js])
  (:import (java.util.concurrent.atomic AtomicLong)
           (java.nio.file Files Paths)))


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


(defn dict-attack
  "See https://bugs.openjdk.java.net/browse/JDK-8072773
   on why the parallel version of this will only work properly on Java 1.9."
  [try-fn ^String dict-path]
  (js/stream-some
    (map try-fn)
    (-> dict-path
        (Paths/get (into-array String []))
        Files/lines
        ;.parallel ;; no point doing that until 1.9
        )))

(comment
  (dict-attack
    #(and (= "1943" %) %)
    "/home/dimitris/Desktop/test-dict.txt"))