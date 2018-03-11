(ns treajure.io
  (:require [clojure.java.io :as jio]
            [clojure.string :as str]
            [clojure.java.shell :as shell])
  (:import [java.io ByteArrayOutputStream FileInputStream OutputStream BufferedWriter]
           (java.nio.file Paths Files)
           (java.util.stream Stream)))

(defn input-stream->byte-array
  "Extracts the contents of input-stream into a byte-array."
  ^bytes [input]
  (with-open [bout (ByteArrayOutputStream.)]
    (jio/copy input bout)
    (.toByteArray bout)))

(defn file->byte-array
  "Extracts the contents file into a byte-array."
  ^bytes [path]
  (with-open [f-in (FileInputStream. (jio/file path))]
    (input-stream->byte-array f-in)))

(defn resource->byte-array
  ^bytes [resource]
  (let [ccl (.getContextClassLoader (Thread/currentThread))]
    (with-open [is (.getResourceAsStream ccl resource)]
      (input-stream->byte-array is))))

(defn spit-bytes
  "Like `clojure.core/spit`, but for bytes."
  [out ^bytes bs]
  (with-open [^OutputStream w (jio/output-stream out)]
    (.write w bs)
    (.flush w)))

(defn process-file-line-by-line
  "Helper for processing a file line-by-line, without allocating a (potentially massive) String (via `slurp`),
   or the whole list of lines in a vector (via `split-lines`).
   Returns a vector (per `mapv`) with the result of applying <f> to each line in the file."
  [fpath f]
  (with-open [rdr (jio/reader fpath)]
    (mapv f (line-seq rdr))))

(defn process-file-lines
  "Helper for processing a file in bulk, without creating a (potentially massive) String (via `slurp`),
   or the whole list of lines in a vector (via `split-lines`).
   Returns whatever is the result of applying <f> to the full list of lines in the file.
   WARNING: As <f> receives an unrealised lazy-seq of lines, it MUST fully realise them within it.
   In other words, do not let <f> be lazy with respect to its consumption of lines."
  [fpath f]
  (with-open [rdr (jio/reader fpath)]
    (f (line-seq rdr))))

(defn writer-newline
  [^BufferedWriter bw]
  (.newLine bw))

(defn write-lines-to-file
  ""
  ([fpath lines]
   (write-lines-to-file fpath true lines))
  ([fpath add-newlines? lines]
  (let [add-newline (if add-newlines?
                      writer-newline
                      (constantly nil))]
    (with-open [wtr (jio/writer fpath)]
      (doseq [^String line lines]
        (.write wtr line)
        (add-newline wtr))))))

(defn file-size-bytes
  "Returns the number of bytes for the specified <source>
   which can be anything compatible with `io/file`."
  [source]
  (.length (jio/file source)))

(defn file-size-chars
  "Returns the number of characters for the specified <source>
   which must be either a String (the file-path).
   ATTENTION: Only works on Linux!"
  [^String source]
  (let [os-name (System/getProperty "os.name")
        shell-command! (if (or (.startsWith os-name "Linux")
                               (.startsWith os-name "Mac")
                               (.startsWith os-name "Solaris"))
                         #(shell/sh "wc" "-m" source)
                         (throw (IllegalStateException. "`file-size-chars` only works on Unix-based OSes (i.e. Linux, MacOS & Solaris)!")))]
    (-> (shell-command!)
        :out
        (str/split #"\s" 2)
        first
        Long/parseLong)))

(defn file-range [fpath start end]
  (let [^Stream line-stream (Files/lines (Paths/get fpath))]
    (-> line-stream
        (.skip start)
        (.limit end)
        )

    )
  )


;===================<CONSOLE PROGRESS-BAR>==========================

(def ^:private rv
  (vec (range 1 51)))

(defn progress-bar [percent]
  (let [buff (StringBuilder. "[")]
    (run!
      #(cond
         (< % (int (/ percent 2))) (.append buff "=")
         (= % (int (/ percent 2))) (.append buff ">")
         :else (.append buff " "))
      rv)
    (.append buff "] ")
    (.append buff percent)
    (.append buff "%")
    (.toString buff)))

(defmacro write-out! [tick]
  `(->> ~tick
        int
        progress-bar
        (str \return)
        .getBytes
        (.write System/out)))


(defmacro with-progress-seq [k expr]

  `(let [init# (/ 100 (count ~(last expr))) ;; have to count the collection
         interval*# (volatile! init#)]

     (time
       (condp = ~k
         ;; e.g. `map`, `keep`, `mapcat`, `filter`, `remove` etc
         :exhaust
         (let [[t# f# coll#] ~(vec expr)
               res# (t# (fn [x#]
                          (let [res# (f# x#)
                                interval# @interval*#]
                            (write-out! interval#)
                            (vreset! interval*# (+ interval# init#))
                            res#))
                        coll#)]
           (println)
           res#)

         ;; e.g. `some`, `not-any`, `every?` etc
         :short-circuit
         (let [[t# f# coll#] ~(vec expr)
               res# (t# (fn [x#]
                          (let [res# (f# x#)
                                interval# @interval*#]
                            (if res#
                              (write-out! 100)
                              (do (write-out! interval#)
                                  (vreset! interval*# (+ interval# init#))))
                            res#))
                        coll#)]
           (println)
           res#
           )

         ;; e.g. `reduce`, `reductions` etc - (with init)
         :reduce
         (let [[t# f# ini# coll#] ~(vec expr)
               res# (t# (fn [x#]
                          (let [res# (f# x#)
                                interval# @interval*#]
                            (if (reduced? res#)
                              (write-out! 100)
                              (do (write-out! interval#)
                                  (vreset! interval*# (+ interval# init#))))
                            res#))
                        ini#
                        coll#)]
           (println)
           res#
           )

         (throw (UnsupportedOperationException.
                  "Only [:exhaust, :short-circuit, :reduce] are supported!"))

         ))
     )
  )


