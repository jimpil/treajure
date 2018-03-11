(ns specs.encore
  (:require [treajure.encore :as enc]
            [clojure.spec.alpha :as spc]
            [clojure.spec.gen.alpha :as gen])
  (:import (java.util.concurrent.atomic AtomicLong)))

(defn- idx-pred
  "Given an integer <n>, returns a predicate fn
   which will return `true` the n+1 time it is called,
   `false` in any other case."
  [n]
  (let [i (AtomicLong. 0)]
    (fn [_]
      (= n (.getAndIncrement i)))))

(defn- extract-k-found-gen
  "Gen override for `::extract-args`, for the case where k is found."
  []
  (gen/bind
    (spc/gen ::persistent-coll)
    #(gen/tuple
       (if (seq %)
         (gen/elements
           (cond
             (map? %) (keys %) ;; all valid keys
             (set? %) %        ;; all valid elements
             (sequential? %) (if (> (rand) 0.5)
                               (range (count %)) ;; all valid indices
                               (map idx-pred (range (count %)))))) ;; valid predicates at all indices
         (gen/any)) ;; nothing can be possibly found in an empty coll
       (gen/return %)))
  )

(defn- extract-k-not-found-gen
  "Gen override for `::extract-args`, for the case where k is not found."
  []
  (gen/bind
    (spc/gen ::persistent-coll)
    #(gen/tuple
       (cond
         (empty? %) (gen/any) ;; nothing can be possibly found in an empty coll
         (map? %)   (gen/any) ;; invalid keys (most likely)
         (set? %)   (gen/any) ;; invalid elements (most likely)
         (sequential? %) (if (> (rand) 0.5)
                            (gen/int) ;; invalid indices (most likely)
                            (gen/return (constantly false))))
       (gen/return %)))
  )

(defn- extract-args-gen []
  (gen/one-of [(extract-k-found-gen)
               (extract-k-not-found-gen)]))


;======================================================================
(spc/def ::anything-but-NaN ;; I can live without NaNs - they totally mess up equality
  (spc/and any? (complement enc/double-NaN?)))

(spc/def ::persistent-map
  (spc/map-of ::anything-but-NaN ::anything-but-NaN))

(spc/def ::persistent-coll
  (spc/or
    :map     ::persistent-map
    :vector (spc/coll-of ::anything-but-NaN :kind vector?)
    :set    (spc/coll-of ::anything-but-NaN :kind set?)
    :list   (spc/coll-of ::anything-but-NaN :kind list?)))

(spc/def ::predicate
  (spc/fspec :args (spc/cat :x any?)
             :ret boolean?))

(spc/def ::extract-args
  (spc/cat :k (spc/or
                :predicate ::predicate
                :key-or-index any?)
           :coll ::persistent-coll))

(spc/fdef enc/extract

  :args (spc/spec ::extract-args
                  :gen extract-args-gen)

  :ret (spc/tuple any? coll?)

  :fn (spc/or
        :some-found #(let [[e c] (:ret %)
                           [coll-type arg-coll] (-> % :args :coll)]
                      (and (some? e)
                           (> (count arg-coll)
                              (count c))
                           #_(do (println "SOME-FOUND - cret=" c "argc=" arg-coll \newline (-> % :args :k second))
                               true)
                           ))
        :nil-found #(let [[e c] (:ret %)
                          [coll-type arg-coll] (-> % :args :coll)]
                     (and (nil? e)
                          (> (count arg-coll)
                             (count c))
                          #_(do (println "NIL-FOUND - cret=" c "argc=" arg-coll \newline (-> % :args :k second))
                              true)
                          ))
        :not-found #(let [[e c] (:ret %)
                          [coll-type arg-coll] (-> % :args :coll)]
                     (and (nil? e)
                          (= arg-coll c)
                          #_(do (println "NOT-FOUND - cret=" c "argc=" arg-coll \newline (-> % :args :k second))
                              true)
                          )))
  )

;;====================================================================
(def ^:private core-predicates ;; as seen in `clojure.spec.gen`
  #{any?
    some?
    number?
    integer?
    int?
    pos-int?
    neg-int?
    nat-int?
    float?
    double?
    boolean?
    string?
    ident?
    simple-ident?
    qualified-ident?
    keyword?
    simple-keyword?
    qualified-keyword?
    symbol?
    simple-symbol?
    qualified-symbol?
    uuid?
    uri?
    decimal?
    inst?
    seqable?
    indexed?
    map?
    vector?
    list?
    seq?
    char?
    set?
    nil?
    false?
    true?
    zero?
    rational?
    coll?
    empty?
    associative?
    sequential?
    ratio?
    bytes?})

(def pred-gens
  (zipmap core-predicates
          (map (comp gen/return #(enc/safe-fn % (constantly false)))
               core-predicates)))

(defn- predicate-from-core [exclusions]
  (gen/one-of
    (vals (apply dissoc pred-gens exclusions))))

(spc/fdef enc/compact

  :args (spc/cat :m ::persistent-map
                 :compactors (spc/*
                               (spc/spec ::predicate
                                         ;; cannot use `empty?` because the outer map might become empty BECAUSE OF compaction,
                                         ;; which can lead to a value satisfying a compactor but apparently having stayed in.
                                         ;; e.g.:  (compact {:a {[] []}} empty?) => {:a {}}
                                         :gen (partial predicate-from-core [empty?])))) ;; need proper predicates (ones that look at the actual argument and decide accordingly)!
  :ret ::persistent-map

  :fn (spc/or
        :compacted (fn [{args :args ret :ret}]
                     (let [ret-vals (enc/deep-vals ret)]
                      (and (not-any? nil? ret-vals) ;; default compactor
                           (not-any? (fn [compactable?]
                                       (some compactable? ret-vals)) ;; there shouldn't be anything left to compact
                                     (:compactors args)))))
        :not-compacted #(= (:ret %) ;;standard-case
                           (-> % :args :m))
        )


)
;==============================================================

(spc/fdef enc/separate
   :args (spc/cat :pred (spc/spec ::predicate
                                  :gen (partial predicate-from-core nil))
                  :coll ::persistent-coll)
   :ret (spc/tuple sequential? sequential?)

   :fn (fn [{args :args ret :ret}]
         (let [p? (:pred args)
               [coll-type arg-coll] (:coll args)
               f (juxt (partial filter p?) ;; 2-pass equivalent of `separate`
                       (partial remove p?))]
           (= ret (f arg-coll))))

)

;;================================================

(spc/fdef enc/all-paths
   :args (spc/cat :m ::persistent-map)

   :ret sequential?

   :fn (fn [{args :args ret :ret}]
         (let [arg-coll (:m args)]
           (= arg-coll
              ;;reconstruct arg-coll from :ret
              (reduce #(assoc-in %1 %2 (get-in arg-coll %2)) ;; pick the values from the original coll
                      {}
                      ret))))

          )


(spc/fdef enc/rand-long
  :args (spc/and (spc/cat :start pos-int?
                          :end pos-int?)
                 #(< (:start %)
                     (:end %)))

  :ret pos-int?

  :fn #(let [args (:args %)]
         (spc/int-in-range? (:start args)
                            (:end args)
                            (:ret %)))
  )



(spc/fdef enc/partition-all-randomly
          :args (spc/cat :max (spc/and pos-int? (partial < 3))
                 :coll (spc/and sequential? #(< 3 (count %))))

          :ret (spc/* sequential?) ;; 0 or more partitions

          :fn #(let [args (:args %)
             [mxp original] ((juxt :max :coll) args)
             res (:ret %)
             flat-res (apply concat res)] ;; flatten 1 level (the partitions)
         (and (= original flat-res) ;; we've not lost any elements
              (every? (fn [rp] ;; all partitions' length are below :max
                        (>= mxp (count rp)))
                      res)))

          )

(spc/fdef enc/partition-all-varyingly ;; plain `pos-int?` throws ArithmeticOverflow sometimes
          :args (spc/cat :sizes (spc/coll-of (spc/and pos-int? (partial < 100)) :into []) :coll sequential?)

          :ret (spc/* sequential?) ;; 0 or more partitions

          :fn #(let [args (:args %)
              [sizes original] ((juxt :sizes :coll) args)
              res (:ret %)
              flat-res (apply concat res) ;; flatten 1 level (the partitions)
              shorten-last-size? (> (apply + sizes)
                                    (count original))]
          (and (= original flat-res) ;; we've not lost any elements
               (every? true? ;; partitions are correct
                       (map (fn [size partition]
                              (= size (count partition)))
                            ;; last partition might have been shrunk so it's not easy to check
                            ;; it doesn't matter though - we KNOW we haven't lost any elements.
                            (cond-> sizes shorten-last-size? drop-last)
                            (cond-> res shorten-last-size? drop-last)))))

          )
