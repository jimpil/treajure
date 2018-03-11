(ns treajure.pmap
  "Some utilities for taking advantage of multi-core machines.
   Choose your mapper wisely according to the requirements at hand."
  (:require [clojure.core.reducers :as r]
            [treajure.encore :as c])
  (:import [java.util.concurrent ExecutorService ExecutorCompletionService TimeUnit CancellationException ScheduledExecutorService]))

;; ---------------------------------------------
;; TASK/COLL | small    | large       | infinite
;; ---------------------------------------------
;; cheap     |`pool-map`|`pool-map`   |`mapr`
;; expensive |`pmap`    |`pool-map`   |`mapr`
;; variable  |`rhmap`   |`rhmap`      |`mapr`
;;----------------------------------------------
;;
;;

(defonce cpu-no (.. Runtime getRuntime availableProcessors))

(defonce time-units
  {:days  TimeUnit/DAYS
   :hours TimeUnit/HOURS
   :minutes  TimeUnit/MINUTES
   :seconds  TimeUnit/SECONDS
   :microseconds TimeUnit/MICROSECONDS
   :milliseconds TimeUnit/MILLISECONDS
   :nanoseconds  TimeUnit/NANOSECONDS
   })

(defn do-after*
  "Schedules <f> (a fn of no args) for execution after <ms> milliseconds.
   You typically use this to trigger a side effect after some delay (for 0 delay you can simply use `future`).
   Returns a function of no arguments to unschedule <f> if possible (which depends on a number of things).
   Most commonly, the task will NOT execute if it's cancelled before having been started.
   Otherwise, the code you're running must be actively checking for the interrupt flag."
  [dlay ^ScheduledExecutorService pool f]
  (let [^ScheduledExecutorService pool (or pool (c/thread-pool :scheduled-solo nil))
        fut (.schedule pool ^Callable f (long dlay) TimeUnit/MILLISECONDS)]
    #(future-cancel fut)))

;;==============>PARALLEL-MAPPERS<=====================================

(defn pool-map
  "A saner, more disciplined (reuses threads) version of 'pmap'.
   Submits jobs eagerly but polls for results lazily.
   If <maintain-order?> is false, an `ExecutorCompletionService`
   will be used to maximise throughput.
   A global timeout (applicable only to tasks that have NOT yet started) is supported
   via the last arg (destructured as `[timeout-millis timeout-fn]`) in the 4-arg arity.
   <timeout-fn> must be a function of 1 arg (the CancellationException object).
   If nil, or if a non-fn value is provided a fn will be constructed via `constantly`.
   In both cases, a constant value will always be returned (`:timeout` for nil - the constant value otherwise)."
  ([f coll]
   (pool-map f (+ 2 cpu-no) coll))
  ([f threads coll]
   (pool-map f threads false coll))
  ([f threads ordered? coll]
   (pool-map f threads ordered? nil coll))
  ([f threads maintain-order? [timeout-ms timeout-res] & colls]
   (let [exec (c/thread-pool :fixed threads)
         pool (cond-> exec ;;use a completion-service to maximize throughput
                      (not maintain-order?) (ExecutorCompletionService.))
         submit* (if maintain-order? ;;  we have to do this dance to avoid reflection
                  (fn [& xs]
                    (.submit ^ExecutorService pool ^Callable #(apply f xs)))
                  (fn [& xs]
                    (.submit ^ExecutorCompletionService pool #(apply f xs))))
         futures (try (apply mapv submit* colls)  ;;be eager on the inside
                      (finally (.shutdown exec)))
         timeout-res-fn (when (number? timeout-ms)
                          (c/->fn (or timeout-res :timeout)))]
     ;; schedule the timeout only if <timeout-ms> is sensible
     (when timeout-res-fn
       (do-after* timeout-ms nil
                  #(run! future-cancel futures)))
     ;;be lazy on the outside
     (if maintain-order?
       ;; ask for results in the original order
       (map (if timeout-res-fn
              #(try (deref %)
                    ;; guard against the global timeout
                    (catch CancellationException ce
                      (timeout-res-fn ce)))
              deref)
            futures)
       ;; ask for results as soon as they finish
       (repeatedly (count futures)
                   (if timeout-res-fn
                     #(try (.. ^ExecutorCompletionService pool take get)
                         ;; guard against the global timeout
                         (catch CancellationException ce
                           (timeout-res-fn ce)))
                     #(.. ^ExecutorCompletionService pool take get)))))))


(defn- fold-into-vec [chunk coll]
  "Provided a reducer <coll>, concatenate into a vector.
   Same as (into [] coll), but parallel."
  (r/fold chunk (r/monoid into vector) conj coll))

(defn- shuffled?-vector
  "Optionally shuffles <coll> and converts it to a vector (if it isn't already)."
  [coll shuffle?]
  (cond-> coll
          shuffle? shuffle
          (not (vector? coll)) vec))

(defn rmap
  "A fork-join based mapping function that uses vectors underneath.
  Returns a vector with the results of mapping <f> over <coll>."
  ([f coll fj-chunk-size shuffle?]
   (fold-into-vec fj-chunk-size
                  (r/map f (shuffled?-vector coll shuffle?))))
  ([f coll fj-chunk-size]
   (rmap f coll fj-chunk-size false))
  ([f coll]
   (rmap f coll 512)) )

(defn rhmap
  "A high-performance, fork-join based mapping function similar to rmap, which however, uses ArrayLists underneath.
   You can expect pedal-to-the-metal performance with this (assuming you pick the right <fj-chunk-size>).
   Returns a vector with the results of mapping <f> over <coll."
  ([f coll fj-chunk-size shuffle?]
   (into []
         (r/fold fj-chunk-size r/cat r/append!
                 (r/map f (shuffled?-vector coll shuffle?)))))
  ([f coll fj-chunk-size]
   (rhmap f coll fj-chunk-size false))
  ([f coll]
   (rhmap f coll 512)) )

(defn mapr
  "A pretty basic map-reduce style mapping function.
   Also supports shuffling which defaults to false.
   Will partition the data according to <p-size> and assign a future to each partition (per pmap).
   Assumes a large <coll> and a cheap <f> (the opposite assumptions of 'pmap').
   Use this when you want parallelism but can't avoid laziness.
   If your <coll> is insanely large and you happen to know its size, consider providing a <p-size>,
   otherwise the default is 512. Returns a lazy-seq."
  ([f coll p-size shuffle?]
   (->> (cond-> coll shuffle? shuffle)
        (partition-all p-size)
        (pmap #(mapv f %))
        (apply concat))) ;;concat the inner vectors that represent the partitions
  ([f coll p-size]
   (mapr f coll p-size false))
  ([f coll]
   (mapr f coll 512)))


