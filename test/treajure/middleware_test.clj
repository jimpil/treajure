(ns treajure.middleware-test
  (:require [treajure.middleware :refer :all]
            [clojure.test :refer :all])
  (:import (java.util.concurrent.atomic AtomicLong)))


(deftest wrap-with-middleware-tests
  (let [request (AtomicLong. 0)
        handler #(doto % (.get))
        m1 (fn [h]
             #(doto (h %)
               (.incrementAndGet)))
        m2 (fn [h]
             #(doto (h %)
               (.decrementAndGet)))
        wrapped-handler (with-middleware handler m1 m2)] ;; m2 is called first

    (is (zero? (wrapped-handler request))) ;;  0 => -1 => 0
    )
  )
