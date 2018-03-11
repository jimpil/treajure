(ns treajure.trance-test
  (:require [clojure.test :refer :all]
            [treajure.trance :as trance])
  (:import (java.util.stream LongStream)))

(deftest iterator-reducible-test
  (is (= (iterator-seq (.iterator (LongStream/range 0 50)))
         (into []
               (trance/iterator-reducible (.iterator (LongStream/range 0 50))))))
  )





