(defproject score-generator "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[instaparse "1.4.10"]
                 [org.clojure/clojure "1.10.0"]
                 [hiccup "1.0.5"]]
  :repl-options {:init-ns score-generator.core})