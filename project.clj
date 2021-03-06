(defproject om-svg-tut "0.1.0-SNAPSHOT"
  :description "Sudoku Game in SVG"
  :url "http://wbabic.github.io"

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/tools.reader "0.8.16"]
                 [org.clojure/tools.analyzer.jvm "0.6.5"]
                 [org.clojure/clojurescript "0.0-3126"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [org.omcljs/om "0.8.8"]
                 [org.clojure/core.logic "0.8.10"]]

  :plugins [[lein-cljsbuild "1.0.4"]]

  :source-paths ["src" "target/classes"]

  :clean-targets ["out/om_svg_tut" "main.js"]

  :cljsbuild {
   :builds [{:id "dev"
             :source-paths ["src"]
             :compiler {
                        :main om-svg-tut.core
                        :output-to "main.js"
                        :output-dir "out"
                        :optimizations :none
                        :cache-analysis true
                        :source-map true}}]})
