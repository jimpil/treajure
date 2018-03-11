(ns treajure.compression-test
  (:require [clojure.test :refer :all]
            [treajure.compression :refer :all])
  (:import (java.util Arrays)))


(deftest zlib-roundtrip
  (let [original-data (byte-array (repeatedly 100000 rand)) ;; 100,000 random bytes
        compressed-data (compress-bytes original-data)] ;; the compressed version
    (is (> (alength original-data) ;; compressed must be significantly less bytes
           (alength compressed-data)))
    (is (Arrays/equals original-data ;; round-tripping back via `decompress` matches original
                       (decompress-bytes compressed-data)))
    )
  )
