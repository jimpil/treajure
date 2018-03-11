(ns treajure.middleware)

(defn with-middleware
  "Helper for wrapping handler <h> with an arbitrary number of <middleware>."
  [handler & middleware]
  (reduce (fn wrap-with*
            [handler wrapper]
            (wrapper handler))
          handler
          middleware))

(defn wrap-with-exception-handling
  "General purpose error-handling middlware."
  ([handler]
   (wrap-with-exception-handling handler println))
  ([handler logger]
   (wrap-with-exception-handling handler logger "There was an error while processing your request..."))
  ([handler log-error default-error-message]
   (fn [request]
     (try
       (handler request)
       (catch Exception e
         (let [edata (ex-data e)]
           (log-error e)
           {:status (:http-code edata 500)
            :body   (:http-body edata default-error-message)}))))))
