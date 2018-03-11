(ns treajure.jlamda
  (:import (java.util.function Predicate Function Consumer BiConsumer BiFunction Supplier LongSupplier IntSupplier
                               LongConsumer IntConsumer UnaryOperator DoubleConsumer DoubleSupplier IntUnaryOperator
                               LongUnaryOperator DoubleUnaryOperator BinaryOperator)))

;;helper macros to remove boilerplate
(defmacro ^:private generate-uni-variant
  [target-iface method]
  `(reify ~target-iface
     (~method [~'_ ~'x]
       (~'f ~'x))))

(defmacro ^:private generate-bi-variant
  [target-iface method]
  `(reify ~target-iface
     (~method [~'_ ~'x ~'y]
       (~'f ~'x ~'y))))


(defmulti jlamda (fn [target-interface f] target-interface))
;;=========================================================
;;#########################################################

;============<PREDICATES>=========

(defmethod jlamda :predicate [_ f]
  (reify Predicate
    (test [_ x]
      (boolean (f x)))))
;=================================
;=========<FUNCTIONS>=============

(defmethod jlamda :function [_ f]
  (generate-uni-variant Function apply))

(defmethod jlamda :bi-function [_ f]
  (generate-bi-variant BiFunction apply))
;===================================
;===========<CONSUMERS>=============

(defmethod jlamda :consumer [_ f]
  (generate-uni-variant Consumer accept))

(defmethod jlamda :long-consumer [_ f]
  (generate-uni-variant LongConsumer accept))

(defmethod jlamda :int-consumer [_ f]
  (generate-uni-variant IntConsumer accept))

(defmethod jlamda :double-consumer [_ f]
  (generate-uni-variant DoubleConsumer accept))

(defmethod jlamda :bi-consumer [_ f]
  (generate-bi-variant BiConsumer accept))
;====================================
;==========<SUPPLIERS>===============

(defmethod jlamda :supplier [_ f]
  (reify Supplier
    (get [_]
      (f))))

(defmethod jlamda :long-supplier [_ f]
  (reify LongSupplier
    (getAsLong [_]
      (long (f)))))

(defmethod jlamda :int-supplier [_ f]
  (reify IntSupplier
    (getAsInt [_]
      (int (f)))))

(defmethod jlamda :double-supplier [_ f]
  (reify DoubleSupplier
    (getAsDouble [_]
      (double (f)))))


#_(let [supported-primitives #{:long :int}]

  (defn- assert-correct-return! [return]
    (assert (contains? supported-primitives return)
            "Only :long & :int primitives are supported!"))

  (defn- prim-identifier [prim]
    (-> prim name (str "-supplier") keyword))

  (defn random-supplier
    "Random integer supplier, based on Math.random()"
    ([n]
     (random-supplier :long n))
    ([return-prim n]
     (assert-correct-return! return-prim)
     (-> return-prim prim-identifier (jlamda (partial rand-int n)))))


  (defn uniform-random-supplier
    "Random (uniformly distributed) integer supplier, based on Random.nextInt(int bound)"
    ([n]
     (uniform-random-supplier nil n))
    ([seed n]
     (uniform-random-supplier :long seed n))
    ([return-prim seed n]
     (assert-correct-return! return-prim)
     (let [r (if seed
               (Random. seed)
               (Random.))]
       (-> return-prim prim-identifier (jlamda #(.nextInt r n))))))
  )
;===================================================================


(defmethod jlamda :unary [_ f]
  (reify UnaryOperator
    (apply [_ x]
      (f x))))

(defmethod jlamda :binary [_ f]
  (reify BinaryOperator
    (apply [_ x y]
      (f x y))))

(defmethod jlamda :int-unary [_ f]
  (reify IntUnaryOperator
    (applyAsInt [_ x]
      (int (f x)))))

(defmethod jlamda :long-unary [_ f]
  (reify LongUnaryOperator
    (applyAsLong [_ x]
      (long (f x)))))

(defmethod jlamda :double-unary [_ f]
  (reify DoubleUnaryOperator
    (applyAsDouble [_ x]
      (double (f x)))))

;;------------------------------------

(comment
  ;; gives => [2 3]
  (-> (doto (java.util.ArrayList.)
        (.add 1)
        (.add 2))
      .stream
      (.map (jlamda :function inc))
      (.collect (java.util.stream.Collectors/toList)))

  (-> [1 2 3 4 5]
      .stream
      (.filter (jlamda :predicate odd?))
      (.collect (java.util.stream.Collectors/toList)))


  )
