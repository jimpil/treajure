(ns treajure.encore
  "Functions general enough to be viewed an extension of `clojure.core`.
   Any function prefixed with `m` is meant to deal with maps.
   Similarly, a `+` suffix is meant to communicate some kind of minor improvement
   over existing `clojure.core` fns, whereas a `++` means
   that the syntax has been completely reworked."
  (:require [clojure.string :as str]
            [treajure
             [jlamda :as jl]
             [atomic :as atomic]
             [macros :as macros]]
            [clojure.set :as set]
            [treajure.jlamda :as jlamda])
  (:import (java.util.concurrent.locks Lock ReentrantReadWriteLock StampedLock ReentrantLock)
           (java.util UUID Properties ArrayList Arrays)
           (java.util.regex Pattern)
           (java.util.concurrent Future LinkedBlockingQueue CompletableFuture ExecutorService Executors ThreadFactory TimeUnit)
           (java.util.stream Stream LongStream)
           (java.text SimpleDateFormat)
           (clojure.lang IPersistentMap IPersistentSet Sequential IEditableCollection Atom)
           (java.lang.reflect Array)
           (java.security SecureRandom)))

;==========================<SEQS>================================

(defn keepv
  "Like `clojure.core/keep` specialised to return vector."
  [f coll]
  (into [] (keep f) coll))

(defn nextv
  "Like `clojure.core/next`, specialised for vectors.
   Delegates to `clojure.core/subvec`."
  [v]
  (subvec v 1))

(defn first-which
  "Returns the first item in <coll>
   for which `(pred item)` returns truthy value."
  [pred coll]
  (some #(when (pred %) %) coll))

(defn removev
  "The opposite of `clojure.core/filterv`."
  [pred coll]
  (filterv (complement pred) coll))

(defn map-while
  "Returns a lazy-seq of successive items from `(map f coll)`
   for as long as `(pred item)` returns truthy value."
  ([f pred]
   (comp (map f)
         (take-while pred)))
  ([f pred coll & colls]
   (apply sequence (map-while f pred) coll colls)))

(defn mapv-while
  "Same as <map-while>, but returns a vector."
  [f pred coll & colls]
  (if (seq colls)
    (into [] (apply map-while f pred coll colls))
    (into [] (map-while f pred) coll))) ;; optimised version when dealing with 1 <coll>

(defn seq-contains?
  "Like `clojure.core/contains?` but for seqs."
  [coll x]
  (some (partial = x) coll))

(defn unchunk
  "Create a one-at-a-time sequence from a chunked sequence."
  [s]
  (lazy-seq
    (when-let [x (first s)]
      (cons x (unchunk (rest s))))))

(defn zipvec
  "Think `zipmap` but instead of a 'table' (map) of fns => xs,
   returns a vector of function applications: `[(f1 x1) (f2 x2) ... (fn xn)]`.
   If one collection is shorter, then the loop will stop (per `mapv`)."
  [fns xs]
  (mapv #(%1 %2) fns xs))

(defn juxtzip
  "Takes a number of <fns> (f1 f2 ... fn) and returns a new function F, which takes
   a number of of <args> (x1 x2 ... xn), and returns a vector `[(f1 x1) (f2 x2) ... (fn xn)]`.
   Kind of similar to `clojure.core/juxt`, but each fn receives a different argument
   (the element from <args> at the same index as itself in <fns>)."
  [& fns]
  (fn [& args]
    (zipvec fns args)))

(defn separate
  "Splits items in <coll> in 2 groups, according to the result of `(pred item)` (truthy vs falsey)
   Returns a vector of 2 vectors. First inner vector contains items that 'passed' <pred>,
   whereas items that didn't are contained in the second vector.
   Everything happens in one pass. Not lazy!"
  [pred coll]
  (if (seq coll)
    (let [groups (group-by (comp boolean pred) coll)
          yes (get groups true [])
          no (get groups false [])]
      [yes no])
    [[] []]))

(defn splitv-at
  "Same as `clojure.core/split-at`, but assumes a vector as the <collv>,
   and therefore can return vectors (via `subvec`), rather than seqs (via `take`/`drop`)."
  [n collv]
  (if (seq collv)
    [(subvec collv 0 n)
     (subvec collv n)])
  collv)

(defn zipmap+
  "Same as `clojure.core/zipmap`, but uses transients."
  [keys vals]
  (loop [map (transient {})
         ks (seq keys)
         vs (seq vals)]
    (if (and ks vs)
      (recur (assoc! map (first ks) (first vs))
             (next ks)
             (next vs))
      (persistent! map))))


(defprotocol IExtract
  "Abstraction for extracting a value out of a persistent collection."
  (extract* [this k]))

(extend-protocol IExtract

  IPersistentMap
  ;; for extracting the first map-entry which satisfies a predicate <k>,
  ;; call `seq` on the map before passing it to `extract`
  (extract* [this k]
    [(get this k)
     (dissoc this k)])
  ;; for extracting the first item which satisfies a predicate <k>,
  ;; call `seq` on the set before passing it to `extract`
  IPersistentSet
  (extract* [this k]
    [(get this k)
     (disj this k)])

  Sequential
  (extract* [this k] ;; if <k> is a predicate, extract the first element that satisfies it
    (if (int? k)
      (if (neg? k)
        [nil this]
        (let [[left right] (split-at k this)
              e (first right)]
          [e (concat left (next right))])
        )
      (let [[left right] (split-with (complement k) this)
            e (first right)]
        [e (concat left (next right))])
      )
    )

  ArrayList ;; this is the only case where the source data-structure (`java.util.ArrayList` in this case) is mutated (per `.remove`)
  (extract* [this k]
    (if (int? k)
      (if (neg? k)
        [nil this]
        (let [ii (int k)]
          (try [(.get  this ii)
                (doto this
                  (.remove ii))]
               (catch IndexOutOfBoundsException _
                 [nil this])))
        )
      (reduce  ;; if <k> is a predicate, extract the first element that satisfies it
        (fn [res i]
          (let [ii (int i)
                e (.get this ii)]
            (if (k e)
              (reduced [e (doto this
                            (.remove ii))])
              res)))
        [nil this]
        (range (.size this)))
      )
    )
  )

(defn extract
  "Analogous to `clojure.core/get`, but returns a vector of `[item-at-k, coll-without-k]`.
   For seqs <k> can be an integer (the index), or a predicate.
   In case of a predicate, the first item that satisfies it will be extracted.
   Handles Maps, Sets, Sequentials & ArrayLists."
  [k coll]
  (extract* coll k))

(defn aextract
  "Same as `extract`, but for arrays. <k> can be an index or a predicate.
   Returns a new array as the second element (does not mutate <a>)."
  [k a]
  (if-let [aseq (seq a)]
    (-> aseq
        (extract* k)           ;; delegate to the impl for Sequential
        (update 1 into-array)) ;; turn the second element into an array
    [nil a]))


;==========================<MAPS>================================

(defn all-paths
  "Given a map <m>, returns a list of all its paths."
  [m]
  (when (map? m)
    (mapcat (fn [[k v]]
              (let [sub (all-paths v)
                    nested (map (partial apply conj [k])
                                (filter seq sub))]
                (if (seq nested)
                  nested
                  [[k]])))
            m)))

(defn deep-vals
  "Analogous to `clojure.core/vals`, but traverses the map until it bottoms out (1-arg overload).
   The 2-arg version expects the specific paths to pull vals from."
  ([m]
   (deep-vals m (all-paths m)))
  ([m paths]
   (map (partial get-in m) paths)))

(defn dissoc-in
  "Analogous to `clojure.core/assoc-in`."
  [x path]
  (update-in x (butlast path) dissoc (last path)))

(defn mirror-keys
  "Strips from <m> any keys not contained in <mirror> (via `select-keys`)."
  [mirror m]
  (select-keys m (keys mirror)))

(defn mirror-meta
  "Returns <target> with <source>'s metadata (if any)."
  [source target]
  (if-let [smeta (meta source)]
     (with-meta target smeta)
     target))

(defn take-meta
  "Returns <target> with <source>'s metadata (if any) merged with its own."
  [source target]
  (if-let [smeta (meta source)]
    (vary-meta target merge smeta)
    target))

(defn move-to-meta
  "Move some of the keys from m into its metadata, overriding existing values.
   (move-to-meta {:a 1 :b 2} [:a]) -> ^{:a 1} {:b 2}"
  [m & ks]
  (-> (apply dissoc m ks)
      (vary-meta merge (select-keys m ks))))


;; helpers for working with maps (prefixed with 'm')

(defn filter-keys
  "Filters entries from <m>, where `(pred key)`, returns logical true."
  [pred m]
  (when m
    (select-keys m (filter pred (keys m)))))

(defn remove-keys
  "Removes entries from <m>, where `(pred key)`, returns logical true."
  [pred m]
  (when m
    (select-keys m (remove pred (keys m)))))

(defn filter-vals
  "Removes entries from <m>, where `(pred value)`, returns logical false."
  [pred m]
  (when m
    (persistent!
      (reduce-kv (fn [m k v]
                   (if (pred v)
                     m
                     (dissoc! m k)))
                 (transient m)
                 m))))

(defn remove-vals
  "Removes entries from <m>, where `(pred value)`, returns logical true."
  [pred m]
  (filter-vals m (complement pred)))


(defn map-vals
  "Maps <f> to all values of <m>."
  [f m]
  (persistent!
    (reduce-kv (fn [m k v]
                 (assoc! m k (f v)))
               (transient {})
               m)))

(defn map-entries
  "Maps <f> to all map-entries of <m> (per `clojure.core/map`)."
  [f m]
  (into {} (map f) m))

(defn keep-entries
  "Keeps <f> to all map-entries of <m> (per `clojure.core/keep`)."
  [f m]
  (into {} (keep f) m))


(defn map-keys
  "Maps <f> to all keys of <m>."
  [f m]
  (persistent!
    (reduce-kv (fn [m k v]
                 (assoc! m (f k) v))
               (transient {})
               m)))

(defn update-vals
  "Returns <m> with values at keys <ks> updated via `(apply update m k f args)`.
   Sequence of keys are also supported, in which case `update-in` will be used.
   Returns a new map."
  [m ks f & args]
  (reduce (fn [m k]
            (apply (if (sequential? k)
                     update-in
                     update)
                   m k f args))
          m
          ks))


(defn update-keys
  "Returns <m> with keys <ks> updated via `(apply f k args)`.
   Sequence of keys are also supported, in which case `get/assoc/dissoc-in` will be used.
   Returns a new map."
  [m ks f & args]
  (reduce (fn [m k]
            (let [[get* dissoc* assoc* k* path*] (if (sequential? k)
                                          [get-in dissoc-in assoc-in (last k) (vec (butlast k))]
                                          [get dissoc assoc k nil])
                  old-v (get* m k)]
              (if (some? old-v)
                (-> m
                    (dissoc* k)
                    (assoc* (if (sequential? k)
                              (conj path* (apply f k* args))
                              (apply f k* args))
                            old-v))
                m)))
          m
          ks))

(defn update!
  "Same as `clojure.core/update` but for transient collections."
  ([m k f]
    (assoc! m k (f (get m k))))
  ([m k f x]
    (assoc! m k (f (get m k) x)))
  ([m k f x y]
    (assoc! m k (f (get m k) x y)))
  ([m k f x y z]
    (assoc! m k (f (get m k) x y z)))
  ([m k f x y z & more]
    (assoc! m k (apply f (get m k) x y z more))))

(defn update-in!
  "Same as `clojure.core/update-in` but for transient collections.
   WARNING: Assumes <m> is transient all the way down!"
  [m [k & ks] f & args]
  (if ks
    (assoc!  m k (apply update-in (get m k) ks f args))
    (assoc! m k (apply f (get m k) args))))


(defn update-vals!
  "Same as `update-vals`, but for transient collections.
   If some key form <ks> is something sequential (denoting a path),
   it is assumed that all levels down that path are transient too (per `update-in!`)."
  [m ks f & args]
  (reduce (fn [m k]
            (apply (if (sequential? k)
                     update-in!
                     update!)
                   m k f args))
          m
          ks))

(defn merge!
  "An implementation of `merge` which expects two arguments (both transients)."
  [x y]
  (reduce-kv
    (fn [res k v]
      (assoc! res k v))
    x
    y))

(defn merge-with!
  "An implementation of `merge-with` which expects all maps to be transients."
  [f & maps]
  (when (some identity maps)
    (let [merge-entry (fn [m e]
                        (let [[k v] ((juxt key val) e)]
                          (if (contains? m k)
                            (assoc! m k (f (get m k) v))
                            (assoc! m k v))))
          merge2 (fn [m1 m2]
                   (reduce merge-entry (or m1 (transient {})) (persistent! m2)))]
      (reduce merge2 maps))))

(defn safe-fn
  "Returns a var-arg function which will call <f> with the given arguments,
   whithin a try-catch expression. If an exception is caught, <ex-res> (an arbitray expression),
   will be evaluated."
  [f ex-res-fn]
  (fn [& args]
    (macros/safely ex-res-fn (apply f args))))

(defn- compact*
  [m compact?]
  (reduce-kv
    (fn [source k v]
      (if (compact? v)
        source
        (if (map? v)
          (assoc source k (compact* v compact?))
          (assoc source k v))))
    {}
    m))

(defn compact
  "Recursively removies entries that correspond to nil value, from map <m>.
   The 2-arg version (and above) supports extra compaction criteria via extra <compactors> predicates."
  ([m]
   (compact m nil))
  ([m compactor & compactors]
   (assert (map? m) "<compact> expects a map as the first argument!")
   (let [compact? (if compactor
                    (->> compactors
                         (cons compactor)
                         (map #(safe-fn % (constantly false)))
                         (apply some-fn))
                    (constantly false))]
     (compact* m (fn [v]
                   (or (nil? v)
                       (compact? v)))))))

(defn select&rename-keys
  "A combination of `clojure.core/select-keys`
   and `clojure.set/rename-keys`."
  [m keyseq renames]
  (-> m
      (select-keys keyseq)
      (set/rename-keys renames)))

;==========================<BOOLEANS>================================

(defn atom? [x]
  (instance? Atom x))

(def truthy? boolean)

(def falsey?
  (comp false? boolean))

(defn re-pattern?
  "Returns true if <p> is an instance of `java.util.regex.Pattern` - false otherwise."
  [p]
  (instance? Pattern p))

(defn array?
  "Returns true if <x> is an array (of any type) - false otherwise."
  [x]
  (and x (.isArray (.getClass x))))

(defn transient?
  "Returns true if the given collection <coll> is a transient one - false otherwise."
  [coll]
  (instance? IEditableCollection coll))

(defn lock?
  "Returns true if <l> is an instance if `java.util.concurrent.locks.Lock` - false otherwise."
  [l]
  (instance? Lock l))

(defn double-NaN?
  "Returns true if <d> is Double/NaN, false otherwise."
  [d]
  (and (double? d)
       (Double/isNaN d)))

(defn float-NaN?
  "Returns true if <d> is Float/NaN, false otherwise."
  [x]
  (and (float? x)
       (Float/isNaN x)))

(defn arrays-deep-equals?
  [ax ay]
  (Arrays/deepEquals ax ay))

;======================================================================

(defn class+ [x]
  (let [c (class x)]
    [c (.getName c)]))

(defn invoke-constructor
  "Wrapper around `clojure.core/construct-proxy` which can handle class names
   as strings (fully qualified). Returns an instance of the class <c>."
  [c & args]
  (apply construct-proxy
         (cond-> c (string? c) Class/forName)
         args))


(defmacro on-shutdown!
  "Registers a function containing <body> as a shutdown hook."
  [& body]
  `(.addShutdownHook (Runtime/getRuntime)
                     (Thread. ^Runnable (fn [] ~@body))))


(defmacro time+
  "Execute body and record time taken.
   Returns a vector of 2 elements that can be destructured as
   [body-return-value, duration (in milliseconds)]."
  [& body]
  `(let [before# (System/nanoTime)
         ret# (do ~@body)
         after# (System/nanoTime)]
     [ret# (/ (- after# before#)
              1000000.0)])) ;; nanos => millis

(defn time++
  "A composable (non-macro) version of `time+`.
   Instead of arbitrary code, it expects a no-arg fn."
  [f]
  (time+ (f)))

(defmacro locking+
  "Like `clojure.core/locking`, but expects a `java.util.concurrent.locks.Lock` <lock>."
  [lock & body]
  `(try
     (.lock ~lock)
     ~@body
     (finally (.unlock ~lock))))

(defn lock
  "Creates a new instance of `java.util.concurrent.locks.Lock`,
   given a <policy> keyword. Options are:

   :rrw => ReentrantReadWriteLock
   :stamped => StampedLock

   In any other case you get a ReentrantLock.
   <fair?> is expected to be a boolean and it
   only applies to reentrant locks."
  ([]
   (lock nil))
  ([policy]
   (lock policy false))
  ([policy fair?]
   (case policy
     :rrw (ReentrantReadWriteLock. fair?)
     :stamped (StampedLock.)
     (ReentrantLock. fair?))))


(defn ->fn
  "Coerce <x> to a function (via `(constantly x)`),
   unless it is already one (tests via `fn?`)."
  [x]
  (if (fn? x)
    x
    (constantly x)))

(defn future-cancel+
  "Like `clojure.core/future-cancel` but also allows to specify
   an <interrupt?> param (per `Future.cancel(boolean interrupt)`),
   which defaults to false in the 1-arg arity."
  ([fut]
   (future-cancel+ fut false))
  ([^Future fut interrupt?]
   (.cancel fut interrupt?)))


(defmacro fn-call?
  "Returns true if <form> represents a normal function call - false otherwise.
   Useful in macros."
  [form]
  (and (seq? form)
       (let [sym (first form)]
         (and (symbol? sym)
              (let [v (resolve sym)]
                (and (var? v)
                     (bound? v)
                     (not (:macro (meta v)))
                     (fn? (var-get v))))))))


(defn map->properties
  "Converts a map <m> to a `java.util.Properties` object."
  ^Properties [m]
  (doto (Properties.)
    (.putAll m)))

(defn properties->map
  ([props]
   (into {} props))
  ([props kf]
   (map-keys kf (properties->map props))))

(defn java-version
  "Returns a map with 3 keys [:major, :minor, :vendor].
   Example: For Oracle Java 1.8.0_91, you get:
   {:major \"1.8\"
    :minor \"0_91\"
    :vendor \"Oracle Corporation\"}"
  []
  (let [vs (System/getProperty "java.version")
        vnd (System/getProperty "java.vendor")
        [x y minor] (str/split vs #"\.")]
    {:major (str x \. y)
     :minor minor
     :vendor vnd}))

(defn uuid
  "Returns a random UUID string (per `UUID/randomUUID`)."
  ^String []
  (.toString (UUID/randomUUID)))

(defn uuids [n]
  (repeatedly n uuid))

(defn pipe
  "Returns a vector containing a sequence that will read from the
   queue, and a function that inserts items into the queue.
   Source: http://clj-me.cgrand.net/2010/04/02/pipe-dreams-are-not-necessarily-made-of-promises/"
  []
  (let [q (LinkedBlockingQueue.)
        EOQ (Object.)
        NIL (Object.)
        s (fn queue-seq []
            (lazy-seq (let [x (.take q)]
                        (when (not= EOQ x)
                          (cons (when (not= NIL x) x)
                                (queue-seq))))))]
    ;; returns the sequence, and a function to put things into that sequence
    [(s) (fn queue-put
           ([] (.put q EOQ))
           ([x] (.put q (or x NIL))))]))

(defmacro future+
  "Like `clojure.core/future`, but returns a CompletableFuture available in JDK8.
   <callback> should be a function of 1 arg (the completion result), or nil. "
  [callback & body]
  `(let [cb# ~callback]
     (cond-> (CompletableFuture/supplyAsync (jl/jlamda :supplier (fn [] ~@body)))
             cb# (.thenApplyAsync (jl/jlamda :function cb#)))))

(defn stream->seq
  "Turns a `java.util.stream.Stream` to a seq (via its iterator)."
  [^Stream s]
  (-> s
      .iterator
      iterator-seq))

(defn thread-pool
  ""
  (^ExecutorService [named-type n-threads]
   (thread-pool named-type n-threads (Executors/defaultThreadFactory)))
  (^ExecutorService [named-type n-threads factory]
   (let [^ThreadFactory factory factory]
     (case named-type
       :scheduled (Executors/newScheduledThreadPool n-threads factory)
       :scheduled-solo (Executors/newSingleThreadScheduledExecutor factory)
       :fixed (if (= 1 n-threads)
                (recur :solo n-threads factory)
                (Executors/newFixedThreadPool n-threads factory))
       :cached (Executors/newCachedThreadPool factory)
       :solo (Executors/newSingleThreadExecutor factory)))))

(def ^SimpleDateFormat RFC3339-datetime-formatter
  (SimpleDateFormat. "yyyy-MM-dd'T'HH:mm:ss.SSSZ"))

(defn now!
  "Returns the current time (nanoseconds by default).
   Options for <precision> are [:micros :millis :seconds :minutes :hours].
   <decimal-precision?> is about whether you want a Double or a Long returned.
   "
  ([]
   (now! :nanos))
  ([precision]
   (now! precision false))
  ([precision decimal-precision?]
   (let [now (System/nanoTime)
         nano-converter TimeUnit/NANOSECONDS]
     (case precision
       :micros (if decimal-precision?
                 (/ now 1000.0)
                 (.toMicros nano-converter now))
       :millis (if decimal-precision?
                 (/ now  1000000.0)
                 (.toMillis nano-converter now))
       :seconds (if decimal-precision?
                  (/ now  1000000000.0)
                  (.toSeconds nano-converter now))
       :minutes (if decimal-precision?
                  (/ now  1000000000000.0)
                  (.toMinutes nano-converter now))
       :hours (if decimal-precision?
                (/ now 1000000000000000.0)
                (.toHours nano-converter now))
       now
       ))))


(defn sorted-frequencies
  "Same as `clojure.core/frequencies` but returns a sorted-map."
  [coll]
  (let [ninc (fnil unchecked-inc 0)]
    (reduce (fn [freqs x]
              (update freqs x ninc))
            (sorted-map)
            coll)))

(defn freqsort
  "A sorting technique based on `clojure.core/frequencies` + sorted-map (for the actual sorting),
   and transducers (for the final concatenation), exhibiting surprisingly decent performance."
  ([xs]
   (freqsort xs :asc))
  ([xs order]
   (into [] (comp (map (fn [[x n]]
                         (repeat n x))) ;; the x occurred n times
                  cat)
         (cond-> (sorted-frequencies xs)
                 (= order :desc) rseq))))


;; encore API for unifying atom-like things (i.e. atoms with AtomicLong, AtomicInteger & AtomicBoolean objects)
;; includes `trade!` `inc!` & `dec!`, in addition to the standard `swap!+`, `reset!+` & `deref+`

(def ^:private specialisers
  {:long :long-unary
   :int :int-unary})

(defmacro swapper
  "Helper macro for creating swapping/trading lamdas
   out of plain clojure fns. Useful when you want to
   call `swap!+` and/or `trade!` on AtomicLong/Integer
   without paying for the cost of creating the lamda
   upon every single invocation. Optimisation activated
   when <t> is either :long or :int.
   Example:
    (let [atl (AtomicLong. 2)]
      (repeat 20 (swap!+ atl + 2)) ;; => constructs the lamda wrapping `+` 20 times
      (repeat 20 (swap!+ atl (swapper :long + 2)))) ;; => constructs the lamda wrapping `+` once"
  [t f & args]
  (if-let [specialiser (get specialisers t)]
    (case (count args)
      0 `(treajure.jlamda/jlamda ~specialiser ~f)
      1 `(treajure.jlamda/jlamda ~specialiser #(~f % ~(first args)))
      2 `(treajure.jlamda/jlamda ~specialiser #(~f % ~(first args) ~(second args)))
      `(treajure.jlamda/jlamda ~specialiser #(apply ~f % ~args)))
    `(partial ~f ~@args))
  )



(defn swap!+
  "Drop-in replacement for `clojure.core/swap!`.
   Also works AtomicLong & AtomicInteger objects,
   in which case consider preconstructing the appropriate
   Lamda (Long/IntUnaryOperator), otherwise it will be created
   upon every single invocation. See `swapper` for help.
   ALWAYS prefer calling this wrapper over `treajure.atomic/swap!`"
  ([atom f]
   (atomic/swap! atom f))
  ([atom f x]
   (atomic/swap! atom f x))
  ([atom f x y]
   (atomic/swap! atom f x y))
  ([atom f x y & args]
   (atomic/swap! atom f x y args)))

(defn trade!
  "Like `clojure.core/swap!` but returns the vale that was swapped out.
   Works on atoms, but also AtomicLong & AtomicInteger objects,
   in which case consider preconstructing the appropriate
   Lamda (Long/IntUnaryOperator), otherwise it will be created
   upon every single invocation. See `swapper` for help.
   Works on AtomicBoolean objects too, but <f> MUST be a fn of no arguments
   (`boolean` will be called on its result before passing it on to Java).
   ALWAYS prefer calling this wrapper over `treajure.atomic/trade!`"
  ([atom f]
   (atomic/trade! atom f))
  ([atom f x]
   (atomic/trade! atom f x))
  ([atom f x y]
   (atomic/trade! atom f x y))
  ([atom f x y & args]
   (atomic/trade! atom f x y args)))


(defn reset!+
  "Drop-in replacement for `clojure.core/reset!`.
   Also supports AtomicLong, AtomicInteger, AtomicBoolean
   & Semaphore (resets the number of permits) objects."
  [atom newval]
  (atomic/reset! atom newval))

(defn inc!
  [atom]
  "Increases the (presumably numerical) value of <atom> by 1 & returns it.
   AtomicLong, AtomicInteger & Semaphore objects are supported too."
  (atomic/inc! atom))

(defn dec!
  [atom]
  "Decreases the (presumably numerical) value of <atom> by 1 & returns it.
   AtomicLong, AtomicInteger & Semaphore objects are supported too."
  (atomic/dec! atom))

(defn deref+
  "Drop-in replacement for `clojure.core/deref`.
   Also supports AtomicLong, AtomicInteger & AtomicBoolean objects
   via its 1-arg arity."
  ([ref]
   (atomic/deref ref));; pass-through call for atoms
  ([ref timeout-ms timeout-val]
   (deref ref timeout-ms timeout-val))) ;; go via clojure.core



(defmacro with-error-handling
  "Wraps <body> in a `try` catching Throwable
   and passing it to <handle-error> fn."
  [handle-error & body]
  `(try
     ~@body
     (catch Throwable t#
       (~handle-error t#))))

(defn rand-long
  "Drop-in replacement for `clojure.core/rand-int`,
   with an extra arity which returns a random integer
   between <start> (inclusive) & <end> (exclusive)."
  ([end] ;; between 0-end
   (long (rand end)))
  ([start end] ;; between start-end
   (+ start (long (rand (- end start))))))

(defn- random-partitions
  ([^long top]
   (fn [rf]
     (let [rs (volatile! (repeatedly (partial rand-long 1 top)))
           a (ArrayList. top)]
       (fn
         ([] (rf))
         ([result]
          (let [result (if (.isEmpty a)
                         result
                         (let [v (vec (.toArray a))]
                           ;;clear first!
                           (.clear a)
                           (unreduced (rf result v))))]
            (rf result)))
         ([result input]
          (.add a input)
          (if (= (first @rs)
                 (.size a))
            (let [v (vec (.toArray a))]
              (.clear a)
              (vswap! rs rest)
              (rf result v))
            result))))))
  ([top coll]
   (lazy-seq
     (when-let [s (seq coll)]
       (let [n (rand-long 1 top)
             p (doall (take n coll))]
         (cons p (random-partitions top (nthrest s n))))))))


(defn partition-all-randomly
  "Returns <coll> (lazily) partitioned in random
   length chunks - from 1 up to <max-size> (inclusive).
   No elements will be lost (think `partition-all`)
   The 1-arg arity returns a transducer.."
  ([max-size]
   (random-partitions (unchecked-inc max-size)))
  ([max-size coll]
   (random-partitions (unchecked-inc max-size) coll)))


(defn- sized-partitions
  ([sizes]
   (fn [rf]
     (let [ssizes (volatile! sizes)
           a (ArrayList.)]
       (fn
         ([] (rf))
         ([result]
          (let [result (if (.isEmpty a)
                         result
                         (let [v (vec (.toArray a))]
                           ;;clear first!
                           (.clear a)
                           (unreduced (rf result v))))]
            (rf result)))
         ([result input]
          (.add a input)
          (if (= (first @ssizes)
                 (.size a))
            (let [v (vec (.toArray a))]
              (.clear a)
              (vswap! ssizes rest)
              (rf result v))
            result))))))
  ([sizes coll]
   (lazy-seq
     (when-let [s (seq coll)]
       (let [n (first sizes)
             p (doall (take n coll))]
         (cons p (sized-partitions (rest sizes) (nthrest s n))))))))

(defn partition-all-varyingly
  "Returns <coll> (lazily) partitioned according to <sizes>,
   which is expected to be a list of integers
   (the presumably varying partition sizes).
   No elements will be lost (think `partition-all`).
   <sizes> will be cycled. This will be visible only if
   the provided ones add up to less than `(count coll)`.
   If they add up to more, some (n-last) <sizes> will never be used.
   The 1-arg arity returns a transducer."
  ([sizes]
   (sized-partitions (cycle sizes)))
  ([sizes coll]
   (if (empty? sizes)
     (list coll)
     (sized-partitions (cycle sizes) coll))))

(defmacro doseq-indexed
  "Like `clojure.core/doseq`, but with an additional binding to the index (first arg)."
  [index-sym [item-sym coll] & body]
  `(doseq [[~item-sym ~index-sym] (map vector ~coll (range))]
     ~@body))

(defn aget-obj
  "Like `clojure.core/aget` but without requiring a type-hint.
   A good option for non-primitive arrays."
  [a i]
  (Array/get a (int i)))


(defn atype
  "Returns the Class of the elements in array <a>,
   or nil if <a> is not an array."
  [a]
  (.getComponentType (.getClass ^Object a)))


(defn arange
  "Similar to `clojure.core/range`, but returning a long-array."
  (^longs [start end]
   (.toArray (LongStream/range start end)))
  (^longs [end]
   (arange 0 end)))


(defn aconcat
  ""
  [& arrays]
  (let [sizes (map alength arrays)
        sizes-r (vec (reductions + sizes))
        offsets (cons 0 (pop sizes-r))
        total (peek sizes-r)
        out (-> arrays
                first
                atype
                (make-array total))]
    (dorun
      (map #(System/arraycopy %1 0 out %2 %3) arrays offsets sizes))
    out))


(defn either-or
  "Similar to `clojure.core/or` but takes only 2 args,
   and has added semantics. In particular, only one of
   the args can/must be truthy, otherwise nil is returned.
   Think about mutual exclusivity (XOR)."
  [x y]
  (let [xor? (if x
               (not y)
               (boolean y))]
    (when xor?
      (or x y))))

(defn lazy-shuffle
  "A functional/semi-lazy shuffling algorithm.
   Returns a lazy-seq which works against an underlying
   vector <v>. The returned lazy-seq grows at the same rate
   as <v> shrinks (1 element at each step)."
  [v] ;; only works on vectors
  (when (seq v)
    (lazy-seq
      (let [idx (rand-int (count v))]
        (->> v
             peek
             (assoc v idx)
             pop
             lazy-shuffle
             (cons (v idx)))))))


(defn take-until
  "Returns a lazy sequence of successive items from <coll> until
  `(pred item)` returns true, including that item. `pred` must be free of side-effects.
   Returns a transducer when no collection is provided."
  ([pred]
   (fn [rf]
     (fn
       ([] (rf))
       ([result] (rf result))
       ([result input]
        (if (pred input)
          (ensure-reduced (rf result input))
          (rf result input))))))
  ([pred coll]
   (lazy-seq
     (when-let [s (seq coll)]
       (if (pred (first s))
         (cons (first s) nil)
         (cons (first s)
               (take-until pred (rest s))))))))


(defn rand-byte
  "Similar to `clojure.core/rand-int`,
   but returns a (signed) byte."
  []
  (-> (range Byte/MIN_VALUE (unchecked-inc-int Byte/MAX_VALUE))
      rand-nth
      byte))


(defmacro with-lock
  ""
  [lock-expr & body]
  `(let [lockee# ~(with-meta lock-expr {:tag 'java.util.concurrent.locks.ReentrantLock})]
     (.lock lockee#)
     (try ~@body
       (finally
         (.unlock lockee#)))))

(defmacro with-try-lock
  "Same as `with-lock`, but uses `tryLock()` instead."
  [lock-expr & body]
  `(let [lockee# ~(with-meta lock-expr {:tag 'java.util.concurrent.locks.ReentrantLock})]
     (and (.tryLock lockee#)
          (try ~@body
            (finally
              (.unlock lockee#))))))