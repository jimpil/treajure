(ns treajure.m2m
  (:require [treajure.encore :as encore]))

(defn add-ignored-arg
  "Given a 1-arg fn, returns a fn of 2 args,
   ignoring the first one in the body."
  [f]
  (fn [_original v]
    (f v)))


(defn- mapping-for
  ""
  [original oldv mapping]
  (let [mapping-val (-> mapping first val)
        [cond-self cond-both :as conds] ((juxt :if :cond) mapping)
        _ (when (every? some? conds)
            (throw (ex-info "`:if` & `:cond` are mutually exclusive (i.e. can't co-exist in the same mapping)!"
                            {:mapping mapping})))
        condition (cond ;; one or none of these 2 will fire
                    cond-self #(cond-self oldv)
                    cond-both #(cond-both original oldv))
        transfom (cond-> mapping-val
                         (some? condition) :transform) ;; `:transform` can be a map as well
        new-entry  [(-> mapping first key)
                    (transfom oldv)]]
    (assert (ifn? transfom) "No transformation found!")

    (if (nil? condition) ;; unconditional mapping
      new-entry
      (when (condition)
        new-entry)))
  )

(defn m2m
  ""
  [m mappings]
  (encore/keep-entries
    (fn [[k mapping]]
      (when-let [old-v (get m k)]
        (cond ;; there are 3 valid cases
          (sequential? mapping) ;; one-of map mappings
          (or (some (partial mapping-for m old-v)
                    mapping)
              [k old-v]) ;; no conditional matched
          (map? mapping) ;; map mapppings
          (or (mapping-for m old-v mapping)
              [k old-v])
          :else ;; direct value mapping
          [k mapping])))
    mappings))


(comment

  ;; a mapping is either an atom-like value (1),
  ;; a map with a single entry (2),
  ;; or a sequence of maps with a single entry (3)


  ;; 1) [:a 10] denotes a simple value-mapping (e.g. map the value at :a to value 10 ignoring what was there before)
  ;; 2) [:b {:B inc}] denotes a dependent value mapping (e.g. map the value at :b to the increment of what was there before)
  ;; 2.1) [:c {:C {"ha" "he"}}] same as #2 but uses the map as a fn (i.e. there exists some straight mapping from what was there before to something new)
  ;; 3) denotes OR-like semantics for multiple #2 cases. Conditionals are specified in the :if/:cond key and transform-fns in the :transform key
  ;; The first :if/:cond that succeeds takes precedence



  {:a 10
   :b {:B inc}
   :c {:C {"ha" "he"}}
   :d [{:D {:if pos?
            :transform inc}}
       {:G {:if dec?
            :transform dec}}
       {:K {:cond (fn [o v]
                    (and (= 1 (get o :a))
                         (pos? v)))
            :transform inc}}
       ]}

  )