(ns treajure.environment
  (:require [clojure.string :as str]
            [treajure.encore :as enc]))

(defn- ->kw
  [s]
  (-> s
      str/lower-case
      (str/replace #"[_ \.]" "-")
      keyword))

(defn- system-x [x kw?]
  (if kw?
    (enc/properties->map x ->kw)
    (enc/properties->map x)))

(defn- system-env
  [kw?]
  (system-x (System/getenv) kw?))

(defn- system-props
  [kw?]
  (system-x (System/getProperties) kw?))


(defn get-env
  ([]
   (get-env true))
  ([kw?]
  (merge (system-props kw?)
         (system-env kw?))))
