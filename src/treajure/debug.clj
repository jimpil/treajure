(ns treajure.debug)

(defmacro spy
  "Useful for debugging purposes.
   <output> is the fn that will do the actual side-effect,
   which defaults to `println`."
  ([x]
   `(spy clojure.core/println ~x))
  ([output x]
   `(let [x# ~x]
      (~output
        (str "Spying line " ~(or (-> &form meta :line)
                                "n/a") " in " ~*file*
             \newline '~x " =>" \newline x#))
      x#)))


(defmacro spy-env []
  (let [ks (keys &env)]
    `(prn (zipmap '~ks ~ks))))
