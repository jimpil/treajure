(ns treajure.jstreams-test
  (:require [clojure.test :refer :all]
            [treajure.jstreams :as jstreams]
            [treajure.trance :as trance])
  (:import (java.util.stream LongStream)
           (java.util Collections)))

(deftest stream-reducible-tests
  (let [test-stream (jstreams/stream-reducible
                      (LongStream/range 0 200))]
    (is (= 1 (trance/some (map inc) test-stream))))

  (let [test-stream1 (.parallel (LongStream/range 0 200))
        test-stream2 (.parallel (.stream (Collections/nCopies 200 nil)))]
    ;; we're not in a position to know which thread's value we'll see
    (is (integer? (jstreams/stream-some (map inc) test-stream1)))
    ;; all nils - nothing found
    (is (nil? (jstreams/stream-some (map identity) test-stream2))))

  (let [raw-stream (LongStream/range 0 200)]
    (is (= (range 1 201)
           (jstreams/stream-into [] (map inc) raw-stream))))


  (let [raw-stream (.parallel (LongStream/range 0 200))]
    (is (= (range 1 201)
           (jstreams/stream-into [] (map inc) raw-stream))))
  )
