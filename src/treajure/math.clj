(ns treajure.math
  (:require [clojure.pprint :as ppr]
            [treajure.jlamda :as jlamda])
  (:import (java.util Arrays)))

(defn sum
  "Return sum of `(f x)` for each x in <xs>.
   Uses auto-promoting math."
  ([xs f]
   (transduce (map f) +' xs))
  ([xs]
   (reduce +' xs)))

(defmacro ^:private genasum
  ([t f axs]
   `(-> ~axs
        Arrays/stream
        (.map (treajure.jlamda/jlamda ~t ~f))
        (.sum)))
  ([axs]
   `(-> ~axs
        Arrays/stream
        .sum))
  )

(defn asum-doubles
  ""
  ([^doubles axs f]
   (genasum :double-unary f axs))
  ([^doubles axs]
   (genasum axs)))

(defn asum-longs
  ""
  ([^longs axs f]
   (genasum :long-unary f axs))
  ([^longs axs]
   (genasum axs)))

(defn mean
  "Given some numerical values <xs>, returns their mean."
  ([xs]
   (mean xs (count xs)))
  ([xs n]
   (if (pos? n)
     (/ (if (coll? xs)
          (sum xs)
          (xs)) ;; can use an HOF which closes over the xs
        n)
     0)))

(defn amean-doubles
  "Like `mean` but specialised for an array of doubles."
  ([^doubles xs]
   (amean-doubles xs (alength xs)))
  ([xs n]
   (if (pos? n)
     (/ (if (coll? xs)
          (asum-doubles xs)
          (xs)) ;; can use an HOF which closes over the xs
        n)
     0)))

(defn amean-longs
  "Like `mean` but specialised for an array of longs."
  ([^longs xs]
   (amean-longs xs (alength xs)))
  ([xs n]
   (if (pos? n)
     (/ (if (coll? xs)
          (asum-longs xs)
          (xs)) ;; can use an HOF which closes over the xs
        n)
     0)))

(defn mean-squared
  "Given some numerical values <xs>, returns their mean-squared."
  ([xs]
   (mean-squared xs (count xs)))
  ([xs n]
   (mean (partial sum xs #(*' % %)) n)))

(defn amean-squared-doubles
  "Like `mean-squared` but specialised for an array of doubles."
  ([^doubles xs]
   (amean-squared-doubles xs (alength xs)))
  ([xs n]
   (amean-doubles (partial asum-doubles xs #(*' % %)) n)))

(defn amean-squared-longs
  "Like `mean-squared` but specialised for an array of longs."
  ([^doubles xs]
   (amean-squared-longs xs (alength xs)))
  ([xs n]
   (amean-longs (partial asum-longs xs #(*' % %)) n)))

(defn median
  ([xs]
   (median xs (count xs)))
  ([xs n]
  (let [sorted (if (fn? xs)
                 (xs) ;; can use an HOF which encapsulates sorting
                 (sort xs))
        halfway (quot n 2)]
    (if (odd? n)
      (nth sorted halfway) ; In the case that coll has an odd number of items, simply retrieve the nth item
      (let [bottom (dec halfway)
            bottom-val (nth sorted bottom)
            top-val (nth sorted halfway)]
        ;; When coll has an even number of items,
        ;; find the index for the other central value (bottom),
        ;; and take the mean of the top and bottom values
        (mean [bottom-val top-val] 2))))))

(defn amedian-doubles
  ([^doubles xs]
   (amedian-doubles xs (alength xs)))
  ([xs n]
   (let [^doubles sorted (if (fn? xs)
                           (xs) ;; can use an HOF which encapsulates sorting - must return array of doubles
                           (. Arrays (sort xs compare)))
         halfway (quot n 2)]
     (if (odd? n)
       (aget sorted halfway) ; In the case that coll has an odd number of items, simply retrieve the nth item
       (let [bottom (unchecked-dec halfway)
             bottom-val (aget sorted bottom)
             top-val (aget sorted halfway)]
         (mean [bottom-val top-val] 2))))))


(defn amedian-longs
  ([^longs xs]
   (amedian-longs xs (alength xs)))
  ([xs n]
   (let [^longs sorted (if (fn? xs)
                           (xs) ;; can use an HOF which encapsulates sorting - must return array of longs
                           (. Arrays (sort xs compare)))
         halfway (quot n 2)]
     (if (odd? n)
       (aget sorted halfway) ; In the case that coll has an odd number of items, simply retrieve the nth item
       (let [bottom (unchecked-dec halfway)
             bottom-val (aget sorted bottom)
             top-val (aget sorted halfway)]
         (mean [bottom-val top-val] 2))))))

(defn abs-deviations
  ""
  [xs]
  (let [m (median xs)]
    (map #(-' % m) xs)))

;https://en.wikipedia.org/wiki/Median_absolute_deviation
(defn median-abs-deviation
  ""
  [xs]
  (median (abs-deviations xs)))

(defn mode
  "Returns the 'mode' `[most-frequently-occurring-value, n-times-it-occured]`
  in a collection <xs>. Typically used in non-numeric datasets."
  [xs]
  (->> xs
       frequencies
       (apply max-key val)))

(defn basic-stats
  "Returns a map of basic statistics on xs (numbers).
   The map will contain the following keys: [:n :m :m2 :v :std :min :max] which stand for
   [count mean mean-squared variance standard-deviation minimum maximum] respectively."
  [xs]
  (let [n (count xs)
        m (mean xs n)
        m2 (mean-squared xs n)
        med (median xs n)
        median-abs-dev (median-abs-deviation xs)
        v  (- m2 (* m m))
        std (Math/sqrt v)
        min (apply min xs)
        max (apply max xs)]
    {:n n     ; count
     :m m     ; mean
     :m2 m2   ; mean-square
     :med med ; median
     :v v     ; variance
     :std std ; standard deviation
     :abs-dev median-abs-dev ; median absolute deviation
     :min min
     :max max}))

()


