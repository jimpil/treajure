(ns treajure.compression
  (:require [clojure.java.io :as io])
  (:import (java.io ByteArrayOutputStream File)
           (java.util.zip DeflaterOutputStream InflaterInputStream ZipOutputStream ZipEntry)
           (java.util Base64)))

(defn compress-bytes
  "Compress an array of bytes <bs> using the ZLIB compression algorithm.
   Returns the compressed bytes (base64 encoded)."
  ^bytes [^bytes bs]
  (with-open [out (ByteArrayOutputStream.)]
    (doto (DeflaterOutputStream. out)
      (.write bs)
      (.finish))
    (.encode (Base64/getEncoder)
             (.toByteArray out))))

(defn decompress-bytes
  "Decompress an array of bytes <bs> (previously compressed via `compress`)
   using the ZLIB compression algorithm. Returns the decompressed (original) bytes."
  ^bytes [^bytes bs]
  (let [bytes (.decode (Base64/getDecoder) bs)
        out (ByteArrayOutputStream.)]
    (with-open [in (InflaterInputStream. (io/input-stream bytes))]
      (io/copy in out)
      (.toByteArray out))))

(defmacro with-zip-entry
  "Main utility for adding entries to a ZipOutputStream <zip>.
   Wraps with code which sets up and optionally closes a ZipEntry.
   Directory structure can be enforced, but two calls are needed.
   One to create the relevant directory entry (e.g. 'foo/') without closing it,
   and one to add the leaf entry under that directory (e.g. 'foo/bar')."
  [zip entry-name close? & body]
  `(let [^ZipOutputStream zip# ~zip]
     (.putNextEntry zip# (ZipEntry. ^String ~entry-name))
     ~@body
     (flush)
     (when ~close?
       (.closeEntry zip#))))

(defn- do-zip!
  ""
  [^String target-path-prefix ^ZipOutputStream zout files]
  (run! (fn [^File file]
          (if (.isDirectory file)
            (let [dir-name (.getName file)
                  path-so-far (str target-path-prefix dir-name File/separator)]
              ;; first create the directory in the zip
              (with-zip-entry zout path-so-far false nil)
              ;; then pour all its children in
              (do-zip! path-so-far zout (.listFiles file)))
            (with-zip-entry zout (cond->> (.getName file)
                                          target-path-prefix (str target-path-prefix))
                            true
              (io/copy (io/input-stream file) zout))))
        files))


(defn zip!
  "Puts a list of <files> (anything compatible with `clojure.java.io/file`)
   into a .zip file <target-zip>.
   If it doesn't exist it will be created.
   Directory structure is preserved (in case of nested folders)."
  ([target-zip files]
   (zip! nil target-zip files))
  ([^String target-path-prefix ^String target-zip files]

   (when-not (.endsWith target-zip ".zip")
     (throw (IllegalArgumentException. "The target ZIP file must end in '.zip'")))

   (with-open [output (ZipOutputStream. (io/output-stream target-zip))]
     (do-zip! target-path-prefix output (map io/file files)))))
