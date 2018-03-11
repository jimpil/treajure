(ns exercises.concurrency)

(defn fork
  []
  (ref true)) ;; all the forks are free at first

(defn philosopher
  [name forks food-amt]
  (ref {:name name
        :forks forks ;; a pair of forks (refs)
        :eating? false ;; nobody is eating at first
        :food food-amt}))

(defn start-eating
  [phil]
  (dosync
    (if (every? true? (map ensure (:forks @phil)))  ; <-- the essential solution
      (do
        (doseq [f (:forks @phil)]
          (alter f not))
        (alter phil assoc :eating? true)
        (alter phil update :food dec)
        true)
      false)))

(defn stop-eating
  [phil]
  (dosync
    (when (:eating? @phil)
      (alter phil assoc :eating? false)
      (doseq [f (:forks @phil)]
        (alter f not)))))

(defn dine
  [phil retry-interval max-eat-duration max-think-duration]
  (while (and (pos? (:food @phil))
              (not (.isInterrupted (Thread/currentThread))))
    (if (start-eating phil)
      (do
        (Thread/sleep (rand-int max-eat-duration))
        (stop-eating phil)
        (Thread/sleep (rand-int max-think-duration)))
      (Thread/sleep retry-interval))))

;; SIMULATION ENTRY POINT
(defn serve-dinner!
  ([]
   (serve-dinner!
     ["Aristotle" "Kant" "Spinoza" "Marx" "Russell"]))
  ([philosopher-names]
   (let [n (count philosopher-names)
         forks (cycle (repeatedly n fork))
         philosophers (map-indexed
                        #(philosopher %2 [(nth forks %)
                                          (nth forks (inc %))]
                                      1000)
                        philosopher-names)
         feast (mapv #(future (do (dine % 5 100 100)
                                  (println (-> % deref :name)
                                           "has finished his food...")))
                     philosophers)]
     [philosophers forks feast])))

(defn dinner-status
  [[philosophers forks]]
  (dosync
    (doseq [i (range (count philosophers))]
      (let [f @(nth forks i)
            p @(nth philosophers i)]
        (println (str "fork: available=" f))
        (println (str (:name p)
                      ": eating=" (:eating? p)
                      " food=" (:food p)))))))

(defn cancel-dinner!
  [[_ _ feast]]
  (doseq [fut feast]
    (future-cancel fut)))
