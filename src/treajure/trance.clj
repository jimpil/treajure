(ns treajure.trance
  (:require [clojure.java.io :as io]
            [clojure.core.reducers :as reducers]
            [treajure.encore :as encore])
  (:import (java.io BufferedReader)
           (clojure.lang IReduceInit)
           (java.util.concurrent.atomic AtomicLong)
           (java.util Iterator)))

(defn lines-reducible
  "A function like `clojure.core/line-seq`, which however
   returns something reducible instead of a lazy sequence."
  [^BufferedReader r]
  (reify IReduceInit
    (reduce [_ f init]
      (with-open [rdr r]
        (loop [state init]
          (if (reduced? state)
            @state
            (if-let [line (.readLine rdr)]
              (recur (f state line))
              state)))))))


(defn iterator-reducible
  "Similar to `iterator-seq`, but returns something reducible."
  [^Iterator iter]
  (reify IReduceInit
    (reduce [_ f init]
      (loop [it iter
             state init]
        (if (reduced? state)
          @state
          (if (.hasNext it)
            (recur it (f state (.next it)))
            state))))))



(defn process-file-lines
  "A transducer based alternative to `treajure.io/process-file-lines`."
  [fpath xform]
  (->> fpath
       io/reader
       lines-reducible
       (eduction xform)))

(defn process-file-line-by-line
  "A transducer based alternative to `treajure.io/process-file-line-by-line`."
  [fpath f]
  (process-file-lines fpath (map f)))

(defn str!
  "Reducing cousin of `clojure.core/str`.
   Returns a StringBuilder."
  ([]
   (StringBuilder.))
  ([^StringBuilder sb x]
   (.append sb x)))

(def rf-str
  "`transduce` friendly reducing-function for building strings.
  E.g. `(transduce (map inc) rf-str (range 15))` => \"123456789101112131415\""
  (completing str! str))

(defn rf-some
  "Reducing cousin of `clojure.core/some`."
  ([] nil)
  ([x] x)
  ([_ x]
   (when x
     (ensure-reduced x))))

(defn rf-some-gen
  "Generate a custom `some` based reducing fn
   which also checks <f> (a fn of no args)."
  [f]
  (fn
    ([] nil)
    ([x] x)
    ([_ x]
     (when (or (f) x)
       (ensure-reduced x)))))

(def rf-some-interruptible
  "Reducing cousin of `clojure.core/some`, which respects thread interruption."
  (rf-some-gen #(.isInterrupted (Thread/currentThread))))

(defn some
  "Process coll through the specified <xform>
   and return the first logical true value."
  [xform coll]
  (transduce xform rf-some coll))

(defn some-interruptible
  "Process coll through the specified <xform>
   and return the first logical true value,
   or nil if the thread gets interrupted before
   transduction completes."
  [xform coll]
  (transduce xform rf-some-interruptible coll))

(defn rf-last
  "Reducing cousin of `clojure.core/last`."
  ([] nil)
  ([x] x)
  ([_ x] x))

(defn count
  "Count the number of items.
   Either used directly as a transducer
   or invoked with two args as a transducing context."
  ([rf]
   (let [n (AtomicLong.)]
     (fn
       ([] (rf))
       ([acc] (rf (unreduced (rf acc (.get n)))))
       ([acc _] (.incrementAndGet n) acc))))
  ([xform coll]
   (transduce (comp xform count) rf-last coll)))


(defn ptransduce
  "A parallel `transduce` based on `reducers/fold`"
  ([xform rf combinef coll]
   (reducers/fold combinef
                  (xform rf)
                  (cond->> coll
                           (not (vector? coll)) (into []))))
  ([n xform rf combinef coll]
   (reducers/fold n
                  combinef
                  (xform rf)
                  (cond->> coll
                           (not (vector? coll)) (into [])))))

(defn rf-update!
  [f]
  (fn
    ([] (transient {}))
    ([x] x)
    ([state [k v]]
     (encore/update! state k f v))))

(defn map-combiner
  [merge-f]
  (fn
    ([] (transient {}))
    ([x] x)
    ([x y]
     (encore/merge-with! merge-f x y))))


(defn pgroup-by
  "A (potentially) parallel version of `group-by`."
  [kfn coll]
  (persistent!
    (ptransduce (map (juxt kfn identity))
                (rf-update! #(conj (or %1 []) %2))
                (map-combiner into)
                coll)))
