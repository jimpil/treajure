(defproject treajure "0.1.1"
  :description "Clojure kitchen sink (in the good sense)."
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 ;[org.clojure/spec.alpha "0.1.134"]
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 ;[org.clojure/tools.macro "0.1.2"]
                 ;[org.clojure/core.async "0.3.443"]
                 ;[org.clojure/data.priority-map "0.0.7"]
                 ]
  :profiles {:dev {:dependencies [[org.clojure/test.check "0.9.0"]
                                  [criterium "0.4.4"]]}}
  :monkeypatch-clojure-test false
  :jvm-opts ["-Xmx2g" "-server"]
  )
