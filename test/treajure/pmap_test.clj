(ns treajure.pmap-test
  (:require [clojure.test :refer :all]
            [treajure.encore :refer :all]
            [treajure.pmap :refer :all]))


(deftest parallel-tests
  (let [coll1 (range 10000)
        coll2 (range 10000 20000)
        ;; pool-map
        [expected slow] (time+ (doall (pmap + coll1 coll2)))
        [pool fast] (time+ (doall (pool-map + 2 true nil coll1 coll2)))
        ;;mapr
        [expected2 slow2] (time+ (doall (pmap inc coll1)))
        [pr fast2] (time+ (doall (mapr inc coll1)))


        ]
    (println "Slow1 =" slow)
    (println "Fast1 =" fast)
    (println "Slow2 =" slow2)
    (println "Fast2 =" fast2)

    (testing "pool-map"
      (is (= expected pool)) ;; test correctness
      (is (> slow fast)))    ;; test performance

    (testing "mapr"
      (is (= expected2 pr))
      (is (> slow2 fast2)))



    )



  )
