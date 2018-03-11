(ns treajure.codes-test
  (:require [treajure.codes :refer :all]
            [clojure.test :refer :all]))


(deftest uref-factory-tests

  (let [[ncombs uref-produce] (factory {:length 8 :prefix "OK" :cycle-shuffled? true} NUM)
        unique (atom #{})]
    (is (every?
          (fn [uref]
            (println uref)
            (swap! unique conj uref)
            (= 8 (count uref)))
          (repeatedly (* 2 ncombs) uref-produce)))

    ;; uniqueness test - only half made it as expected
    (is (= ncombs (count @unique)))

    )



  (let [[ncombs uref-produce] (factory {:length 8 :prefix "OK" :cycle? true} NUM)]
    (doseq [i (range 1 11)]
      (future ;; 10 consumer threads
        (dotimes [_ 100]
          (let [r (uref-produce)]
            (is (string? r)) ;; we'll never run out
            (println "Thread" i "received" r))
          (Thread/sleep 50)))
      )

    )
  )
