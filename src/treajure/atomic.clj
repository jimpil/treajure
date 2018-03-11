(ns treajure.atomic
  "Drop-in replacements for `swap!`, `reset!` & `deref` that work with primitives found in `java.util.concurrent.atomic`.
   In addition `trade!`, inc! & dec! are provided."
  (:refer-clojure :exclude [swap! reset! deref])
  (:require [treajure.jlamda :as jl]
            ;[treajure.debug :as dbg]
            )
  (:import (java.util.concurrent.atomic AtomicLong AtomicInteger AtomicBoolean)
           (clojure.lang Atom ARef)
           (java.util.concurrent Semaphore)))

(defmacro ^:private optimize [jl f]
  `(if (ifn? ~f)
     ~jl
     ~f))

(defprotocol IAtomic
  (swap! 
    [this f]
    [this f x]
    [this f x y]
    [this f x y more]
    "Same as `clojure.core/swap!`.")
  (trade!
    [this f]
    [this f x]
    [this f x y]
    [this f x y more]
    "Like `clojure.core/swap!`, but returns the value that was swapped out.")
  (reset! [this v]
    "Same as `clojure.core/reset!`.")
  (deref [this]
    "Same as `clojure.core/deref`.")
  (inc! [this]
    "Intended for AtomicInteger/Long, and for atoms holding numbers (uses `unchecked-inc`)")
  (dec! [this]
    "Intended for AtomicInteger/Long, and for atoms holding numbers (uses `unchecked-dec`)"))


(defmacro ^:private trade*
  [ref f & args]
  `(let [validate# (.getValidator ~ref)] ;; extract the validator-fn once
     (loop []
       (let [oldv# (.deref ~ref)
             newv# (~f oldv# ~@args)]
         (when validate#
           (try
             (when-not (validate# newv#)
               (throw (IllegalStateException. "Invalid reference state")))
             (catch Exception e#
               (IllegalStateException. "Invalid reference state" ) e#)))

         (if (.compareAndSet ~ref oldv# newv#)
           (do (.notifyWatches ~ref oldv# newv#)
               oldv#)
           (recur))))))

;; unify atoms with popular 'atomic' objects from the `java.util.concurrent` package
(extend-protocol IAtomic

  Semaphore
  (inc! [this]
    (.acquire this))

  (dec! [this]
    (.release this))

  (reset! [this newval] ;; resets the number of permits
    (.drainPermits this)
    (.acquire newval))

  Atom
  (deref [this]
    (.deref this))

  (swap!
    ([this f]
     (.swap this f))
    ([this f x]
     (.swap this f x))
    ([this f x y]
     (.swap this f x y))
    ([this f x y more]
     (.swap this f x y more))
    )

  (trade!
    ([this f]
     (trade* this f))
    ([this f x]
     (trade* this f x))
    ([this f x y]
     (trade* this f x y))
    ([this f x y more]
     (trade* this #(apply f % x y more)))
    )

  (reset! [this v]
    (.reset this v))

  (inc! [this]
    (.swap this unchecked-inc))

  (dec! [this]
    (.swap this unchecked-dec))

  AtomicLong
  (swap!
    ([this f]
     (.updateAndGet this (optimize
                           (jl/jlamda :long-unary f)
                           f)))
    ([this f x]
     (.updateAndGet this (optimize
                           (jl/jlamda :long-unary #(f % x))
                           f)))
    ([this f x y]

     (.updateAndGet this (optimize
                           (jl/jlamda :long-unary #(f % x y))
                           f)))
    ([this f x y more]
     (.updateAndGet this (optimize
                           (jl/jlamda :long-unary #(apply f % x y more))
                           f))))

  (trade!
    ([this f]
     (.getAndUpdate this (optimize
                           (jl/jlamda :long-unary f)
                           f)))
    ([this f x]
     (.getAndUpdate this (optimize
                           (jl/jlamda :long-unary #(f % x))
                           f)))
    ([this f x y]
     (.getAndUpdate this (optimize
                           (jl/jlamda :long-unary #(f % x y))
                           f)))
    ([this f x y more]
     (.getAndUpdate this (optimize
                           (jl/jlamda :long-unary #(apply f % x y more))
                           f))))

  (reset! [this v]
    (.set this v)
    v)

  (deref [this]
    (.get this))

  (inc! [this]
    (.incrementAndGet this))

  (dec! [this]
    (.decrementAndGet this))

  AtomicInteger
  ;; `swap!` & `trade!` on AtomicInteger incurs at least (when a lamda is provided) the extra cost of the
  ;; `(if (fn? x)...)` optimisation. When a plain fn is provided the cost of constructing
  ;; the lamda should also be considered.
  (swap!
    ([this f]
     (.updateAndGet this (optimize
                           (jl/jlamda :int-unary f)
                           f)))
    ([this f x]
     (.updateAndGet this (optimize
                           (jl/jlamda :int-unary #(f % x))
                           f)))
    ([this f x y]
     (.updateAndGet this (optimize
                           (jl/jlamda :int-unary #(f % x y))
                           f)))
    ([this f x y more]
     (.updateAndGet this (optimize
                           (jl/jlamda :int-unary #(apply f % x y more))
                           f))))

  (trade!
    ([this f]
     (.getAndUpdate this (optimize
                           (jl/jlamda :int-unary f)
                           f)))
    ([this f x]
     (.getAndUpdate this (optimize
                           (jl/jlamda :int-unary #(f % x))
                           f)))
    ([this f x y]
     (.getAndUpdate this (optimize
                           (jl/jlamda :int-unary #(f % x y))
                           f)))
    ([this f x y more]
     (.getAndUpdate this (optimize
                           (jl/jlamda :int-unary #(apply f % x y more))
                           f))))

  (reset! [this v]
    (.set this v)
    v)

  (deref [this]
    (.get this))

  (inc! [this]
    (.incrementAndGet this))

  (dec! [this]
    (.decrementAndGet this))

  AtomicBoolean
  ;; only these 3 are useful and/or meaningful
  (trade! [this f]
    (.getAndSet this (boolean (f))))

  (reset! [this v]
    (.set this v)
    v)
  (deref [this]
    (.get this))

  )
