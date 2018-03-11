(ns treajure.interruptible
  (:refer-clojure :exclude [mapv filterv mapcat keep])
  (:import (java.util.concurrent.atomic AtomicLong)))

(defmacro ^:private interrupted? []
  `(.isInterrupted (Thread/currentThread)))

(defmacro ^:private with-interrupted
  [res & body]
  `(if (interrupted?)
     (reduced ~res)
     ~@body))

(defn mapv
  [f coll & colls]
  (let [n (AtomicLong. 0)
        colls? (seq colls)]
    (persistent!
      (reduce

        (fn [res x]
          (let [index (.getAndIncrement n)]
            (with-interrupted res
              (if colls?
                (->> colls
                   (map #(nth % index))
                   (apply f x)
                   (conj! res))
                (conj! res (f x))))))

        (transient [])
        coll))
    )
  )

(defn filterv
  [f coll]
  (persistent!
    (reduce

      (fn [res x]
        (with-interrupted res
          (if (f x)
            (conj! res x)
            res)))

      (transient [])
      coll))
  )

(defn removev
  [f coll]
  (filterv (complement f) coll))


