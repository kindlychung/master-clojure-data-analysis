(defproject network-six "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[lein-cljsbuild "0.3.2"]]
  :dependencies [[org.slf4j/slf4j-simple "1.7.5"]
                 [org.clojure/clojure "1.6.0"]
                 [org.clojure/data.json "0.2.2"]
                 [org.clojure/tools.trace "0.7.8"]
                 [me.raynes/fs "1.4.4"]
                 [org.clojure/clojurescript "0.0-2202"]]
  :cljsbuild {:builds [{:source-paths ["src-cljs"],
                        :compiler {:pretty-printer true,
                                   :output-to "www/js/main.js",
                                   :optimizations :whitespace}}]})
