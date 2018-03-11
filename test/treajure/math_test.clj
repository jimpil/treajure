(ns treajure.math-test
  (:require
    [clojure.test :refer :all]
    [treajure.math :refer :all]))


(deftest sum-tests

  (testing "summing over Clojure collections"
    (let [data (repeatedly 500 (partial rand-int 1000))]
      (is (== (apply +' data) (sum data)))
      (is (== (apply +' (map inc data)) (sum data inc)))
      )
    )

  (testing "summing over Java arrays"
    (let [ldata (long-array (repeatedly 500 (partial rand-int 1000)))
          ddata (double-array (map double (repeatedly 500 (partial rand-int 1000))))]
      (is (== (apply +' ldata) (asum-longs ldata)))
      (is (== (apply +' (map inc ldata)) (asum-longs ldata inc)))

      (is (== (apply +' ddata) (asum-doubles ddata)))
      (is (== (apply +' (map inc ddata)) (asum-doubles ddata inc)))
      )
    )



 )