(ns treajure.strutils
  (:require [clojure.pprint :as ppr])
  (:import (java.util Base64 Base64$Encoder Base64$Decoder Arrays)
           (java.nio.charset StandardCharsets Charset)
           (java.nio CharBuffer ByteBuffer)))

(defn pad
  "Returns a string which starts/ends with <s> followed/preceded by `(- limit (count s))` <ch> characters."
  (^String [s target-length]
   (pad s target-length \*))
  (^String [s target-length ch]
   (pad s target-length :left ch))
  (^String [^String s target-length side ch]
   (assert (>= (count s) target-length) "Padding window <limit> cannot be greater than the length of the input string!")
   (let [padding (apply str (repeat (- target-length (count s)) ch))]
     (case side
       :left (str padding s)
       :right (str s padding)))))

(defn mask
  (^String [s n]
   (mask s n :left))
  (^String [s n side]
   (mask s n side \#))
  (^String [^String s n side replacement]
   (assert (>= (count s) n) "Masking window <n> cannot be greater than the length of the input string!")
   (let [mask (apply str (repeat n replacement))]
     (case side
       :left (str mask (subs s n (count s)))
       :right (str (subs s 0 n) mask)))))

(defn integer->word
  "Converts an integer to a text representation intended for humans."
  ^String [n]
  (assert (integer? n) "`integer->word` expects an integer!")
  (ppr/cl-format nil "~r" n))

(defn integer->word-with-commas
  "Converts an integer to a string containing commas every three digits."
  ^String [n]
  (assert (integer? n) "`integer->word-with-commas` expects an integer!")
  (ppr/cl-format nil "~:d" n))

(defn capitalize-first
  "Capitalizes the first letter in a string, downcasing all rest characters."
  ^String [^String s]
  (ppr/cl-format nil "~@(~a~)" s))

(defn capitalize-all
  "Capitalize the first letter of every word in a string."
  ^String [^String s]
  (ppr/cl-format nil "~:(~a~)" s))

(defn text-number
  "For numbers 1-9, returns the number spelled out in English.
   Otherwise, returns the number. This follows Associated Press style."
  ^String [n]
  ;; if (< 0 n 10) use the ~R directive, otherwise use ~A
  (ppr/cl-format nil "~:[~a~;~r~]" (< 0 n 10) n))

(defn round-double
  ^String [n precision]
  (ppr/cl-format nil (str "~" precision "$") n))

(defn seq->english
  "Formats a seq in English."
  ^String [s]
  (ppr/cl-format nil "~{~#[<empty>~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~:}" s))

(defn system-properties ^String []
  (ppr/cl-format nil "~30A~A~%~{~20,,,'-A~10A~}~%~{~30A~S~%~}"
                 "Property" "Value" ["" "" "" ""]
                 (->> (System/getProperties)
                      (sort-by key)
                      (mapcat (juxt key val)))))

(defn surround
  ^String [^String s x]
  (str x s x))

(defn- colour-str
  ([code s]
   (colour-str code s false))
  ([code s reset?]
   (str code (if reset?
               (str s "\u001B[0m")
               s))))

(defmacro ^:private gen-colour-fn [colour ccode]
  `(defn ~colour
     ~(str "Prints <x> & <xs> in " colour " colour.")
     [~'x & ~'xs]
     (colour-str ~ccode (apply str ~'x ~'xs))))

(gen-colour-fn black  "\u001B[30m")
(gen-colour-fn red    "\u001B[31m")
(gen-colour-fn green  "\u001B[32m")
(gen-colour-fn yellow "\u001B[33m")
(gen-colour-fn blue   "\u001B[34m")
(gen-colour-fn purple "\u001B[35m")
(gen-colour-fn cyan   "\u001B[36m")
(gen-colour-fn white  "\u001B[37m")

(defmacro with-colour-str
  "Returns a coloured string (via the appropriate ansi escape-code),
   appending a 'reset' code at the end to reinstate the previous colour."
  [c s]
  `(colour-str ~c ~s true))


(defn bytes->base64-str
  "Encodes the specified byte array into a String using the Base64 encoding scheme."
  (^String [^bytes bs]
   (bytes->base64-str bs :plain))
  (^String [^bytes bs encoder]
   (bytes->base64-str bs encoder "UTF-8"))
  (^String [^bytes bs encoder char-encoding]
   (let [^Base64$Encoder enc (case encoder
                               :mime (Base64/getMimeEncoder)
                               :url  (Base64/getUrlEncoder)
                               :plain (Base64/getEncoder)
                               encoder)
         res (.encode enc bs)]
     (String. res (Charset/forName char-encoding)))))

(defn base64-str->bytes
  "Decodes a Base64 encoded String into a byte array using the Base64 encoding scheme."
  (^bytes [^String s]
   (base64-str->bytes s :plain))
  (^bytes [^String s decoder]
   (let [^Base64$Decoder deco (case decoder
                                :mime (Base64/getMimeDecoder)
                                :url  (Base64/getUrlDecoder)
                                :plain (Base64/getDecoder)
                                decoder)]
     (.decode deco s))))

(definline ^:private dist-step
  [pred d index m-weight]
  `(let [[i# j# :as idx#] ~index]
     (assoc ~d idx#
               (cond
                 (zero? (min i# j#))  (max i# j#)
                 (~pred i# j#) (get ~d [(unchecked-dec i#) (unchecked-dec j#)])
                 :else (min
                         (unchecked-inc (get ~d [(unchecked-dec i#) j#])) ;+1 cost for deletion
                         (unchecked-inc (get ~d [i# (unchecked-dec j#)])) ;+1 cost for insertion
                         (+ ~m-weight (get ~d [(unchecked-dec i#) (unchecked-dec j#)])))))))


(defn edit-distance
  "Calculates the 'Levenshtein-distance' between two Strings using efficient bottom-up dynamic programming.
   If s1 & s2 are of equal length and m-weight = 1, the 'Hamming-distance' is essentially calculated."
  ([^String s1 ^String s2 m-weight]
   (let [m (.length s1)
         n (.length s2)
         pred (fn [i j]
                (=
                  (.charAt s1 (unchecked-dec i))
                  (.charAt s2 (unchecked-dec j))))
         step #(dist-step pred %1 %2 m-weight)
         distance-matrix (reduce step {} (for [i (range (unchecked-inc m))
                                               j (range (unchecked-inc n))]
                                           [i j]))]
     (get distance-matrix [m n])))
  ([^String s1 ^ String s2]
   (edit-distance s1 s2 2))) ;+2 cost for substitution usually (per Juramfky's lecture-slides and book)

(defn- chars->bytes
  ""
  (^bytes [encoding cs]
   (chars->bytes false encoding cs))
  (^bytes [stealth? ^String encoding ^chars cs]
   (let [char-buffer (CharBuffer/wrap cs)
         byte-buffer (.encode (Charset/forName encoding) char-buffer)
         ret (Arrays/copyOfRange (.array byte-buffer)
                                 (.position byte-buffer)
                                 (.limit byte-buffer))]
     (when stealth?
       ;; clear potentially sensitive data
       (Arrays/fill (.array byte-buffer) (byte 0)))
     ret)))

(defn bytes->chars
  "Given an array of bytes <bs> and some character-encoding <encoding>
   returns the bytes that correspond to <bs> according to <encoding>
   without leaving any traces of the conversion (e.g. allocating a String object)."
  (^chars [encoding bs]
   (bytes->chars false encoding bs))
  (^chars [stealth? ^String encoding ^bytes bs]
   (let [byte-buffer (ByteBuffer/wrap bs)
         char-buffer (.decode (Charset/forName encoding) byte-buffer)
         ret (Arrays/copyOfRange (.array char-buffer)
                                 (.position char-buffer)
                                 (.limit char-buffer))]
     (when stealth?
       ;; clear potentially sensitive data
       (Arrays/fill (.array char-buffer) \u0000))
     ret)))

