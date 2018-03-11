(ns treajure.codes
  (:require [clojure.math.combinatorics :refer [combinations count-combinations]]
            [treajure.encore :as enc])
  (:import (java.util.concurrent.locks ReentrantLock)))


(def ALPHA
  "The characters [A-Z]."
  (mapv char (range (int \A)
                    (inc (int \Z)))))

(def alpha
  "The characters [a-z]."
  (mapv char (range (int \a)
                    (inc (int \z)))))

(def NUM
  "The numbers [0-9]."
  (range 10))

(defonce SPACE
  [\space])

(def PUNCT
  "The punctuation marks [. , ? ! -]."
  [\. \, \? \! \-])

(def SYM
  "The symbols [> < # ~ % * @]."
  [\> \< \# \~ \% \* \@])

(defn- next-code!
  "Private helper which does the actual mutation,
   and also deals with prefixes/suffixes."
  [prefix suffix refs]
  (when-let [uref (first (enc/trade! refs rest))]
    (apply str prefix (concat uref suffix))))

(defn- adjust-length
  [l prefix suffix]
  (cond-> l
          prefix (- (count prefix))
          suffix (- (count suffix))))

(defn- next-code*
  [shuffle-codes! prefix suffix codes]
  (or (next-code! prefix suffix codes)
      (when shuffle-codes!
        (shuffle-codes!)
        (recur shuffle-codes! prefix suffix codes))))

(defn factory
  "Returns a vector of 2 elements.
   First element is the number of unique
   combinations of <n> elements amongst <allowed>,
   and the second is a no-arg fn which will
   produce the next consecutive element from the
   aforementioned unique combinations. The argument (<cycle?>)
   determines what to do when the elements run out.
   With true, they will never run out, whereas with false the fn
   will eventually return nil (after <n-combs> calls).
   <cycle-shuffled?> has the same effect as <cycle?> with added
   shuffling after each cycle. Use it with caution as shuffling of
   very large seqs can be expensive. When both are true, cycle? supersedes."
  [{:keys [length prefix suffix cycle? cycle-shuffled?]} & allowed]
  {:pre [(pos-int? length)]}

  (let [adjusted-length (cond-> length
                                (or prefix suffix)
                                (adjust-length prefix suffix))
        allowed-all (shuffle (apply concat allowed))
        n-combs (count-combinations allowed-all adjusted-length)
        unique-refs (combinations allowed-all adjusted-length)
        codes (cond-> unique-refs
                      cycle? cycle
                      true atom)
        shuffle-codes (when cycle-shuffled?
                        (let [l (ReentrantLock.)]
                          #(enc/with-try-lock l (reset! codes (shuffle unique-refs)))))]
    [n-combs #(next-code* shuffle-codes prefix suffix codes)]))
