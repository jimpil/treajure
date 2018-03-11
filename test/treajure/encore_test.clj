(ns treajure.encore-test
  (:require [clojure.test :refer :all]
            [clojure.spec.test.alpha :as spct]
            [specs.encore]  ;; no need for alias
            [treajure.encore :refer :all]
            [clojure.string :as str]
            [treajure.macros :as macros])
  (:import (java.util ArrayList Collection Arrays)
           (java.util.concurrent.atomic AtomicLong AtomicInteger)))

(def  gen-test-vars
  "List of Vars to gen-test.
   Add a var in here to have it
   automatically gen-tested."
  [`treajure.encore/extract
   `treajure.encore/compact
   `treajure.encore/separate
   `treajure.encore/all-paths
   `treajure.encore/rand-long
   `treajure.encore/partition-all-randomly
   `treajure.encore/partition-all-varyingly
   ])

(deftest generative-tests
  (testing (str "GENERATIVE TESTING FOR: \n\n"  gen-test-vars)
    (time
      (is (->> {:clojure.spec.test.check/opts {:num-tests 80}}
               (spct/check gen-test-vars)
               spct/summarize-results
               :check-passed
               (= (count gen-test-vars)))))
    )
  )

(deftest extract-test

  (let [data-seq (range 10)
        data-vec (vec data-seq)
        data-set (set data-seq)
        data-map (zipmap data-seq data-seq)
        data-array (into-array data-seq)
        data-List (ArrayList. ^Collection data-vec)
        expected (concat (range 7) (range 8 10))]

    (testing "seqs"
      (let [[e s] (extract 7 data-seq)
            [ee ss] (extract 1 '())
            [eee sss] (extract (partial = 7) data-seq)]
        (is (= 7 e eee))
        (is (= expected s sss))
        (is (nil? ee))
        (is (empty? ss))
        ))

    (testing "vectors"
      (let [[e s] (extract 7 data-vec)
            [ee ss] (extract 1 [])
            [eee sss] (extract 11 data-vec)]
        (is (= 7 e))
        (is (= s expected))
        (is (nil? ee))
        (is (empty? ss))
        (is (nil? eee))
        (is (= data-vec sss))
        ))

    (testing "sets"
      (let [[e s] (extract 7 data-set)
            [ee ss] (extract :a #{})]
        (is (= 7 e))
        (is (= s (set expected)))
        (is (set? s))
        (is (set? ss))
        (is (nil? ee))
        (is (empty? ss))
        ))

    (testing "arrays"
      (let [[e s] (aextract 7 data-array)
            [ee ss] (aextract 1 (into-array []))
            [eee sss] (aextract (partial = 7) data-array)]
        (is (= 7 e eee))
        (is (= (seq s) (seq sss) expected))
        (is (array? s))
        (is (nil? ee))
        (is (Arrays/equals (into-array []) ss))
        ))

    (testing "maps"
      (let [[e s] (extract 7 data-map)
            [ee ss] (extract :a {})]
        (is (= 7 e))
        (is (= s (dissoc data-map 7)))
        (is (map? s))
        (is (map? ss))
        (is (nil? ee))
        (is (empty? ss))

        ))

    (testing "java.util.ArrayLists"
      (let [[e s] (extract 7 data-List)
            [ee ss] (extract 7 (ArrayList.))
            ;; has to be a new one because previous line removed 7 from the original
            [eee sss] (extract (partial = 7) (ArrayList. ^Collection data-vec))]
        (is (= 7 e eee))
        (is (= (seq s) (seq sss) expected))
        (is (instance? ArrayList s))
        (is (instance? ArrayList ss))
        (is (nil? ee))
        (is (empty? ss))
        ))
    )
  )

(deftest compact-tests
  (let [data {:a 1
              :b {:c "hi"
                  :d ""
                  :e nil
                  :t {:o 0}}
              :k ""}
        nil-compacted (dissoc-in data [:b :e])
        string-compacted (-> nil-compacted
                             (dissoc :k)
                             (dissoc-in [:b :d]))
        zero-compacted (dissoc-in string-compacted [:b :t :o])]

    (testing "plain nil compaction (default)"
      (is (= nil-compacted (compact data)))
      )

    (testing "augmented compaction (with `s/blank?`)"
      (is (= string-compacted (compact data str/blank?)))
      )

    (testing "augmented compaction (with `s/blank?` + `zero?`)"
      (is (= zero-compacted (compact data str/blank? zero?)))
      )
    )
  )

(deftest map-while-tests
  (let [data (concat (repeat 5 -2) (repeat 5 1))]
    (is (= (map-while inc neg? data)
           (mapv-while inc neg? data)
           (repeat 5 -1)))
    )
  )


(deftest update-vals-tests
  (let [data {:a {:d 1}
              :b {:e {:h [2 3]}}
              :c {:k {:m {:n 4}}}
              :w 0}]
    (testing "`update-vals` at various levels"
      (is (= {:a {:d 3}
              :b {:e {:h [2 5]}}
              :c {:k {:m {:n 6}}}
              :w 2}
            (update-vals data [:w
                               [:a :d]
                               [:b :e :h 1]
                               [:c :k :m :n]]
                         + 2))))
    )
  )

(deftest update-keys-tests
  (let [data {:a 1
              :b {:c 2}
              :d {:e {:f 3}}}]
    (testing "`update-keys` at various levels"
      (is (= {"a" 1
              :b {"c" 2}
              :d {:e {"f" 3}}}
             (update-keys data [:a [:b :c] [:d :e :f]]
                          name))))
    )

  )

#_(deftest try+-tests
  (is (= "java.lang.ClassCastException: clojure.lang.Keyword cannot be cast to java.lang.Number"
         (macros/try+ (inc :a)
           (catch-all [NumberFormatException ClassCastException] ex
                      (str ex)))))

  )


(deftest atomics-example-based-tests

  (testing "`trade!` & `deref+` with atom, AtomicLong & AtomicInteger"
    (let [state-atom (atom 1)
          state-along (AtomicLong. 1)
          state-aint (AtomicInteger. 1)]

      (is (= 1 (trade! state-atom inc)))
      (is (= 2 (deref state-atom)))

      (is (= 1 (trade! state-along inc)))
      (is (= 2 (trade! state-along + 4 3)))
      (is (= 9 (deref+ state-along)))

      (is (= 1 (trade! state-aint inc)))
      (is (= 2 (deref+ state-aint)))

      )
    )

  (testing "`swap!+` with atom, AtomicLong & AtomicInteger"
    (let [state-atom (atom 1)
          state-along (AtomicLong. 1)
          state-aint (AtomicInteger. 1)]

      (is (= 2 (swap!+ state-atom inc)))

      (is (= 2 (swap!+ state-along inc)))
      (is (= 11 (swap!+ state-along + 5 4)))

      (is (= 2 (swap!+ state-aint inc)))

      )
    )

  (testing "`reset!+` & `deref+` with atom, AtomicLong & AtomicInteger"
    (let [state-atom (atom 1)
          state-along (AtomicLong. 1)
          state-aint (AtomicInteger. 1)]

      (is (= 5 (reset!+ state-atom 5)))
      (is (= 5 (deref+ state-atom)))

      (is (= 5 (reset!+ state-along 5)))
      (is (= 5 (deref+ state-along)))

      (is (= 5 (reset!+ state-aint 5)))
      (is (= 5 (deref+ state-aint)))

      ))

  (testing "optimised `swap!+` & `trade!` for AtomicLong & AtomicInteger objects"
    (let [atl (AtomicLong. 0)
          fast-swap (swapper :long + 1)] ;; constructs the lamda wrapping `(partial + 1)` once

      (time
        (dotimes [_ 60]
          (swap!+ atl fast-swap)))
      (is (= 60 (deref+ atl)))

      (time
        (dotimes [_ 60]
          (dec! atl)))
      (is (zero? (deref+ atl)))

      (time
        (dotimes [_ 60]
          (swap!+ atl + 1))) ;; slow - constructs the lamda wrapping `+` 60 times
      (is (= 60 (deref+ atl)))



      )


    )

  )