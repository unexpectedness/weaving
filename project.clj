(defproject weaving "0.1.2"
  :description "Combinators to weave Clojure functions together"
  :url "https://github.com/unexpectedness/weaving"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [arity "0.1.0"]]
  :profiles {:test {:dependencies
                    [[net.clojars.unexpectedness/shuriken "0.13.30"]]}
             :dev  {:dependencies
                    [[net.clojars.unexpectedness/shuriken "0.13.30"]
                     [codox-theme-rdash "0.1.2"]]}}
  :plugins [[lein-codox "0.10.3"]]
  :codox {:source-uri "https://github.com/unexpectedness/weaving/" \
                      "blob/{version}/{filepath}#L{line}"
          :metadata {:doc/format :markdown}
          :themes [:rdash]})
