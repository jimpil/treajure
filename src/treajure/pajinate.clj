(ns treajure.pajinate)

(defmacro ^:private assert-integers
  [x y]
  `(assert (and (pos-int? ~x)
                (pos-int? ~y))
           "`:page-number` & `:page-size` must be positive integers!"))

(defn- up-to-page-n-plus-one
  [page-size page-number]
  (-> (or page-number 1)
      (* page-size) ;; fetch everything up to the <page-number> page
      inc))

(defmulti limit-query*
  (fn [target-db query page-size]
    target-db))

(defmethod limit-query* :postgres [_ query page-size page-number]
  (assert-integers page-number page-size)
  (str query " LIMIT " (up-to-page-n-plus-one page-size page-number)))

(defmethod limit-query* :oracle [_ query page-size page-number]
  (assert-integers page-number page-size)
  (str "SELECT * FROM (" query ") WHERE ROWNUM <= " (up-to-page-n-plus-one page-size page-number)))


(defn limit-query-for
  [target-db query-string & {:keys [page-size page-number]
                             :or {page-size 10
                                  page-number 1}}]
  (limit-query* target-db query-string page-size page-number))

(def extract-last
  (juxt pop peek))

(defmacro with-pagination
  "A macro that is supposed to surround the code responsible for the db-call.
   First 2 args should be the same page-size & page-number used when limiting the query
   (e.g. via `limit-query-for`). Returns a vector of one (no next page),
   or two elements (there is a next page whose first row is the
   second element in the vector - next-page-marker)."
  [ps pn & body]
  `(let [ret# ~@body]
     (assert (vector? ret#)
             (str \` ~'f \` " should return a vector, but it returned " (type ret#) \!))

     (if (> (count ret#) ;; query should always ask for one more row
            (* ~ps ~pn))
       (treajure.pajinate/extract-last ret#) ;; [ret next-page-marker]
       [ret#])                               ;; [ret]
     )
  )

#_(with-page-size 15 1
   ;;inside the following fn we call `limit-query-for` with page-size = 15 and page-number = 1
  (load-transazctions-by 1 2 3 ...))
