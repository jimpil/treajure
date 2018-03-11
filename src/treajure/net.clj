(ns treajure.net
  (:require [clojure.java.io :as io]
            [clojure.data.json :as json]
            [clojure.java.io :as jio])
  (:import (java.net InetAddress URL URLConnection HttpURLConnection)
           (java.io BufferedReader OutputStream PrintWriter OutputStreamWriter Writer Reader StringWriter DataInputStream)
           (java.nio.file Files)))

(defn- url->reader
  ^BufferedReader [^String u]
  (-> u
      URL.
      jio/input-stream
      io/reader))

(defn public-ip
  "Returns the pubic IP address of the machine
   the code is running on. Uses `api.ipify.org`.
   Subject to java.io.IOException per `URL.openConnection()`
   and `BufferedReader.readLine()`."
  ^String []
  (with-open [rdr (url->reader "https://api.ipify.org")]
    (.readLine rdr)))

(defn daily-exchange-rates
  "Daily currency exchange rates provided for free
   by `fixer.io` (relying on the EU central bank).
   <base-currency> can (optionally) be in its a3 code
   (e.g. USD, GBP etc), and defaults to EUR.
   <historical-from> can (optionally) be a date string
   in the form YYY-MM-dd.
   According to the website rates are updated
   around 4PM CET every working day."
  [& {:keys [base-currency historical-from]}]
  (let [fixer-api "http://api.fixer.io/"
        full-url (if historical-from
                   (str fixer-api historical-from)
                   (cond-> (str fixer-api "latest")
                           base-currency (str "?base=" base-currency)))]
    (with-open [rdr (url->reader full-url)]
      (json/read-str (.readLine rdr)))))

(defn host-address
  ""
  ^String []
  (.getHostAddress (InetAddress/getLocalHost)))

(defn hostname
  ""
  ^String []
  (.getHostName (InetAddress/getLocalHost)))



(defmulti http-post
          (fn [what url data opts]
            what))

(defn- extract-response-body
  [^BufferedReader r]
  (with-open [rdr r]
    (doall
      (take-while some? (repeatedly #(.readLine rdr))))))

(defn- post!
  [^String url {:keys [headers read-response]} out-fn]
  (let [CRLF "\r\n" ;; line separator required by multipart/form-data
        boundary (Long/toHexString (System/currentTimeMillis)) ;; some unique random value
        ^HttpURLConnection conn (.openConnection (URL. url))]
    (.setUseCaches conn false)
    (.setDoOutput conn true) ;; indicates POST method
    (when read-response
      (.setDoInput conn true))  ;; indicates GET method (for the potential response body)
    (.setRequestProperty conn "Content-Type" (str "multipart/form-data; boundary=" boundary))
    (try
      (let [out (.getOutputStream conn)]
        (with-open [wrt (PrintWriter. (OutputStreamWriter. out "UTF-8") true)]
          (-> wrt ;; write the start - this is common for all
              (.append (str "--" boundary))
              (.append CRLF))
          ;; add any headers
          (doseq [[header-k header-v] headers]
            (-> wrt
                (.append (str header-k ": " header-v))
                (.append CRLF)
                (.flush)))

          ;; do the specific bit
          (out-fn wrt out)
          ;; and carry on with the common bits
          (-> wrt
              (.append (str "--" boundary "--"))
              (.append CRLF)
              (.flush))
          ;; fire the request by asking for the response-code
          (when-let [response-code (.getResponseCode conn)]
            {:http-code response-code
             :http-body (when (and (= 200 response-code)
                                   read-response)
                          (read-response (-> conn .getInputStream io/reader)))
             :error-stream (when (and (not= 200 response-code)
                                      read-response)
                             (read-response (-> conn .getErrorStream io/reader)))})))
      (finally
        (.disconnect conn)))
    )
  )


(defmethod http-post :text-file
  [_ url file-path opts]
  (let [{:keys [attr-name charset]
         :or {charset "UTF-8"}} opts]
    (post! url
           opts
           (fn [^Writer wrt ^OutputStream out]
             (let [f (jio/file file-path)
                   fname (.getName f)]
               (-> wrt
                   (.append (format "Content-Disposition: form-data; name=\"%s\"; filename=\"%s\"" attr-name fname))
                   (.append "\r\n")
                   (.append (str "Content-Type: text/plain; charset=" charset)) ;;  Text file must be saved in this charset
                   (.append "\r\n")
                   (.append "\r\n")
                   (.flush))
               (Files/copy (.toPath f) out)
               (.flush out)
               (-> wrt
                   (.append "\r\n") ;; CRLF is important - indicates end of boundary
                   (.flush))))))
  )

(defmethod http-post :binary-file
  [_ url file-path opts]
  (let [{:keys [attr-name]} opts]
    (post! url opts
           (fn [^Writer wrt ^OutputStream out]
             (let [f (jio/file file-path)
                   fname (.getName f)]
               (-> wrt
                   (.append (format "Content-Disposition: form-data; name=\"%s\"; filename=\"%s\"" attr-name fname))
                   (.append "\r\n")
                   (.append (str "Content-Type: " (URLConnection/guessContentTypeFromName fname)))
                   (.append "\r\n")
                   (.append (str "Content-Transfer-Encoding: binary"))
                   (.append "\r\n")
                   (.append "\r\n")
                   (.flush))
               (Files/copy (.toPath f) out)
               (.flush out)
               (-> wrt
                   (.append "\r\n") ;; CRLF is important - indicates end of boundary
                   (.flush))))))
  )


(defmethod http-post :param
  [_ url param opts]
  (let [{:keys [attr-name charset]
         :or {charset "UTF-8"}} opts]
    (post! url opts
           (fn [^Writer wrt _] ;; ignore the 2nd arg (the OutputStream) as it's not needed
             (-> wrt
                 (.append (format "Content-Disposition: form-data; name=\"%s\"" attr-name))
                 (.append "\r\n")
                 (.append (str "Content-Type: text/plain; charset=" charset))
                 (.append "\r\n")
                 (.append "\r\n")
                 (.append ^String param)
                 (.append "\r\n") ;; CRLF is important - indicates end of boundary
                 (.flush)))))
  )


(def ^:private gb5 ;; how many bytes in 5GB?
  (* 1024 1024 1024 5))  ;; 1GB = 1024MB = 1024^2KB = 1024^3B

(defn fileIO-post!
  "Uploads (via http POST method) the file <file-path>
   to https://file.io/ with the specified <lifetime>
   (file will expire after that) - defaults to '14d'."
  [file-path lifetime]
  (assert (or (nil? lifetime)
              (re-matches #"\d[dwmy]" lifetime))
          "<lifetime> must be either nil or a String of the form '\\d[dwmy]'!")

  (assert (>= gb5 (.length (jio/file file-path)))
          "Can NOT upload files larger than 5GB to file.io!")

  (let [url (cond-> "https://file.io"
                    lifetime (str "/?expires=" lifetime))]
    (http-post :text-file url file-path {:attr-name "file"
                                         :read-response extract-response-body})))

(defn fileIO-get!
  "Copies the file specified by <k> from https://file.io/
   to <target> (anything compatible with `io/output-stream`)."
  [k target]
  (with-open [in  (jio/input-stream (URL. (str "https://file.io/" k)))
              out (jio/output-stream target)]
    (jio/copy in out)))

(comment

  (fileIO-post "/home/dimitris/Desktop/creme-brulee.txt" "1d")
  ;;=>
  {:http-code 200,
   :http-body ("{\"success\":true,\"key\":\"fXpLfY\",\"link\":\"https://file.io/fXpLfY\",\"expiry\":\"14 days\"}"),
   :error-stream nil}
  (fileIO-get "fXpLfY" "/home/dimitris/Desktop/creme-bruleee.txt")
  => nil

  )


