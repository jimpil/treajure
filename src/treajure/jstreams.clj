(ns treajure.jstreams
  (:require [treajure.jlamda :as jl]
            [treajure.trance :as trance])
  (:import (java.util.stream Stream StreamSupport)
           (clojure.lang IReduceInit)
           (java.util Spliterator)))

;; Code for consuming java streams.
;; 3 public functions are provided:
;; `stream-reducible`, `stream-into`
;; and `stream-some` (more of a convenience)


(defn- bo-spliterator ;; bailing out Spliterator
  "Wraps a Spliterator such that it will terminate early
  (i.e. when <done?>, a fn of no args, returns true)."
  [^Spliterator internal done?]
  (reify Spliterator
    (characteristics [_]
      (-> internal
          .characteristics
          (bit-and-not Spliterator/SIZED Spliterator/SORTED)))
    (estimateSize [_]
      (if (done?)
        0
        (.estimateSize internal)))
    (tryAdvance [_ action]
      (and (not (done?))
           (.tryAdvance internal action)))
    (trySplit [_]
      (when-let [new-split (.trySplit internal)]
        (bo-spliterator new-split done?)))))

(defn- bo-stream ;; bailing out Stream
  "Wraps a Stream such that it can terminate early
  (i.e. when <done?>, a fn of no args, returns true)."
  ^Stream [^Stream stream done?]
  (let [parallel? (.isParallel stream)]
    (-> stream
        .spliterator
        (bo-spliterator done?)
        (StreamSupport/stream parallel?))))


(defn stream-reducible
  "Turns a Stream into something reducible."
  ([s]
   (stream-reducible s (fn [_ _]
                         (throw
                           (IllegalStateException.
                             "combine-fn NOT provided!")))))
  ([^Stream s combinef]
   (reify IReduceInit
     (reduce [_ f init]
       (let [dp (promise)
             done? (partial realized? dp)
             done  (partial deliver dp true)]
         (with-open [estream (bo-stream s done?)]
           (.reduce estream
                    init
                    ;; accumulator
                    (jl/jlamda :bi-function
                      (fn [acc val]
                        (let [ret (f acc val)]
                          (if (reduced? ret)
                            (do (done) @ret)
                            ret))))
                    ;; combiner
                    (jl/jlamda :binary combinef))))))))

(defn stream-into
  "A 'pouring' transducing context (like `clojure.core/into`), for Java Streams.
   Useful for pouring streams into clojure data-structures
   without involving an intermediate Collection, with the added bonus
   of being able apply a transducer.
   Parallel streams are supported but since `Stream.reduce()`
   doesn't play nicely with mutation (i.e. transients)
   collecting cannot be done fully mutably. `conj` has to be
   the reducing fn per parallel reduction, using `into` only to
   combine. For serial streams we can go fully mutably (much like `.collect()`)."
  ([to stream]
   (if (.isParallel stream)
     (reduce conj to (stream-reducible stream into))
     (into to (stream-reducible stream))))
  ([to xform stream]
   (if (.isParallel stream)
     ;; Cannot use `into` on a parallel stream because it may use transients.
     ;; That goes against the requirement that the reduction
     ;; does not mutate the values received as arguments to combine.
     (transduce xform conj to (stream-reducible stream into))
     ;; for a serial stream we're golden - just delegate to `into`
     (into to xform (stream-reducible stream)))))

(defn stream-some
  "A 'bailing-out' transducing context for Java streams (parallel or not).
   Think `.findFirst()` in terms of Streams, or `clojure.core/some`
   in terms of lazy-seqs."
  ([xform stream]
   (stream-some xform stream identity))
  ([f xform stream]
   (trance/some xform (stream-reducible stream (some-fn f)))))
