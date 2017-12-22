(defproject clojureplayground "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/core.match "0.3.0-alpha5"]
                 [org.clojure/core.async "0.3.465"]]
  :profiles {:dev {:dependencies [[midje "1.9.1"]]
                   :plugins [[lein-midje "3.2.1"]]}}
  :plugins [[lein-cljfmt "0.5.7"]]
  :aliases {"clean-midje" ["do" ["clean"] ["compile"] ["midje"]]})
