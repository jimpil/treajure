(ns treajure.dslog
  (:require [clojure.java.io :as jio])
  (:import (java.io Writer BufferedWriter)
           (java.time LocalDateTime)))

(def format-configs
  {:basic (fn [{:keys [thread source ns]} & args]
            (apply str
                   (LocalDateTime/now)
                   "     " ;; 5 spaces
                   (str "||thread: " (.getName thread) "|ns: " ns "|line: " (:line source)) "||"
                   "     "
                   args))


   }

  )



(defn logger
  ([]
   (logger *out*))
  ([out]
   (logger out true))
  ([out append?]
   (if (instance? Writer out)
     (agent out)
     (agent (jio/writer out :append append?)))))

(defn- write!
  [fmt ^BufferedWriter w contents]
  (let [^String content (->> contents
                             (interpose \space)
                             (apply fmt))]
    (.write w content)
    (.newLine w)))

(defmacro log!
  "Base macro "
  [level fmt logger contents]
  (let [source-details (meta &form)
        ns *ns*]
    `(send-off ~logger (partial write! ~(or fmt str))
               (cons {:thread (Thread/currentThread)
                      :ns ~ns
                      :level (name ~level)
                      :source ~source-details}
                     ~contents))))

(defn log-all!
  [level fmt content & loggers]
  (run! #(log! level fmt % content) loggers))

(defmacro debug [fmt logger & contents]
  `(log! :DEBUG ~fmt ~logger ~contents))

(defmacro debugf [fmt logger content-fmt & args]
  `(debug ~fmt ~logger (apply format ~content-fmt args)))

(defmacro warn [fmt logger & contents]
  `(log! :WARN ~fmt ~logger ~contents))

(defmacro warnf [fmt logger content-fmt & args]
  `(warn ~fmt ~logger (apply format ~content-fmt args)))

(defmacro error [fmt logger & contents]
  `(log! :ERROR ~fmt ~logger ~contents))

(defmacro errorf [fmt logger content-fmt & args]
  `(error ~fmt ~logger (apply format ~content-fmt args)))













