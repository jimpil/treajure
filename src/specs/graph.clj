(ns specs.graph
  (:require [clojure.spec.alpha :as s]))


(s/def ::node
  (s/and fn? #(some-> % meta :deps not-empty)))

(s/def ::graph
  (s/map-of keyword? ;; node-id
            ;; node
            (s/or :init-producer (s/and fn? #(-> % meta :deps empty?))
                  :node ::node)))
