(ns treajure.macros
  (:import (java.util.concurrent.atomic AtomicInteger)))


(defmacro do-while
  "Same as `clojure.core/while`, after having executed <body> once."
  [test & body]
  `(let [f# (fn [] ~@body)]
     (f#)
     (while ~test
       (f#))))


(defmacro while-let
  "Repeatedly executes body while <expr> returns something truthy,
   evaluating <body> with <binding> bound to the value of <expr>."
  [[binding expr] & body]
  `(loop [~binding ~expr]
     (when ~binding
       ~@body
       (recur ~expr))))

(defmacro safely
  "Tries <body>, catching Throwable.
   Returns either whatever <body> returned (nothing was caught),
   or the result of `(exf t)` (something was caught)."
  [exf & body]
  `(try ~@body
        (catch Throwable t#
          (~exf t#))))

(defmacro try++
  "Generates a `try` expression with as many (and in the same order) `catch` clauses as
   `(partition 2 exception-handlers)`, which should be a sequence of the form:
   [ex1 handler-for-ex1
    ex2 handler-for-ex2
    ex3 handler-for-ex3
    :finally finally-expr]
    As shown above, the last entry can, optionally, specify a `finally` clause (a single expression)."
  [exception-handlers & body]
  (assert (and (sequential? exception-handlers)
               (-> exception-handlers count even?))
          "`try++` expects a sequence with an even number of elements as the first arg.")
  (let [gen-catch-clause (fn [[e h]]
                           (let [gs (gensym "t")]
                             (list 'catch e gs (list h gs))))
        [k finally-f] (take-last 2 exception-handlers)
        [ehandlers finally-expr] (if (= :finally k)
                                   [(drop-last 2 exception-handlers) finally-f]
                                   [exception-handlers nil])
        catches (map gen-catch-clause (partition 2 ehandlers))]
    (if finally-expr
      `(try ~@body
            ~@catches
            (finally ~finally-expr))
      `(try ~@body
            ~@catches))
    )
  )

(defn- catch-all*
  "Produces a list of `catch` clauses for all exception classes <exs>
   with the same <catch-tail>."
  [[_catch-all exs & catch-tail]]
  (map #(list* 'catch % (gensym) catch-tail) exs))

(defmacro try+
  "Same as `clojure.core/try`, but also recognises `catch-all` clause(s).
   These must be in the same form as regular `catch`, but instead of a
   single exception class, you are expected to provide a vector of them."
  [& body]
  (let [[true-body catches] (split-with
                              #(-> % first #{'catch 'catch-all} nil?)
                              body)
        catch-all? #(and (seq? %)
                         (= (first %) 'catch-all))
        catch-all-guard (fn [form]
                          (if (catch-all? form)
                            (catch-all* form)
                            [form]))]
    `(try ~@true-body
          ~@(mapcat catch-all-guard catches))))

(defmacro throw-fmt
  "Throws exception <ex> with a message built via `clojure.core/format`."
  [ex msg & fmt-args]
  `(throw (new ~ex (format ~msg ~@fmt-args))))

(defn- local-bindings [env]
  (reduce (fn [m sym]
            (assoc m `'~sym sym))
          {}
          (keys env)))

(defmacro assert+
  "A drop-in replacement for `clojure.core/assert` which comes with more informative output,
   and a map full of details about the failure (per 2nd arg of `ex-info`)."
  [condition error-msg]
  (when *assert*
    `(let [problem# (volatile! nil)
           ret# (try ~condition
                     (catch Throwable t#
                       (vreset! problem# t#)
                       false))]
       (when-not ret#
         (let [assertion-form# (second '~&form)
               sd# ~(meta &form)
               result# (or @problem# ret#)
               line# (:line sd#)
               nsname# '~(ns-name *ns*)
               fname# ~*file*
               printed-error# (str ~error-msg \newline
                                   assertion-form# " => " result# \newline
                                   "Namespace: " nsname# \newline
                                   "File: " fname# \newline
                                   "Line: " line# \newline)]
           (throw (ex-info printed-error# (merge sd#
                                                 {:form assertion-form#
                                                  :local-bindings ~(local-bindings &env)
                                                  :thread-bindings (get-thread-bindings)
                                                  :result result#
                                                  :file fname#
                                                  :ns nsname#})))))
       )
    )
  )

(defmacro assert++
  "Similar to `assert+` but slightly redesigned in order to be more composable.
   If the assertion(s) (specified via <preds>) succeed, <x> is returned.

   Example:

   (assert++ [1 2 3] vector? not-empty (partial every? pos?)
     \"expecting a non-empty vector full of positive numbers\") => [1 2 3]"
  [x & preds]
  (if *assert*
    (let [lexpr (last preds)
          error-msg (when (string? lexpr) lexpr)
          pred-syms (cond-> preds (string? lexpr) butlast)]
      `(let [x# ~x
             exceptions# (volatile! [])
             fail-res# (volatile! [])
             sd# ~(meta &form)
             line# (:line sd#)
             nsname# '~(ns-name *ns*)
             fname# ~*file*
             fidx# (AtomicInteger. 0)
             problem-preds# (remove
                              (fn [pred-sym#]
                                (let [pred-var# (if (list? pred-sym#)
                                                  (eval pred-sym#) ;; handle `comp`, `partial`, anon-fns etc
                                                  (resolve pred-sym#))]
                                  (try
                                    (let [r# (pred-var# x#)]
                                      (when-not r# ;; IF false OR nil
                                        (vswap! fail-res# conj [(.getAndIncrement fidx#) pred-var# r#]))
                                      r#)
                                    (catch Throwable t#
                                      (vswap! exceptions# conj [(.getAndIncrement fidx#) pred-var# (.getMessage t#)])
                                      false))))
                              '~pred-syms)]
         (if (empty? problem-preds#)
           x#
           (let [problem-rets# (sort-by first (concat @fail-res# @exceptions#))
                 problems-str# (->> problem-rets#
                                    (map (fn [[_# problem-pred# problem-ret#]]
                                           (format "(%s %s) => %s" problem-pred# x# problem-ret#)))
                                    (interpose \newline)
                                    (apply str))]
             (throw (ex-info (str "Assertion failure(s): " ~error-msg \newline
                                  problems-str# \newline
                                  "Namespace: " nsname# \newline
                                  "File: " fname# \newline
                                  "Line: " line# \newline)
                             (merge sd#
                                    {:fail-forms (mapv #(list % x#) problem-preds#)
                                     :local-bindings ~(local-bindings &env)
                                     :thread-bindings (get-thread-bindings) ;; FIXME: weird issue when printing!
                                     :v-bind '~x
                                     :v-val x#
                                     :fail-results (mapv last problem-rets#)
                                     :file fname#
                                     :ns nsname#})))))
         ))
    x)
  )

