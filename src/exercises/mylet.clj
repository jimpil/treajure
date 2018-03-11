(ns exercises.mylet)

(defmacro mylet
  "Implement `let` without using `clojure.core/let`."
  [binding-vec & body]
  (assert (and (vector? binding-vec)
               (even? (count binding-vec)))
          "`mylet` expects a binding vector with an even number of bindings...")
  `(apply
     (fn [~@(take-nth 2 binding-vec)] ~@body)
     '~(take-nth 2 (rest binding-vec))))
