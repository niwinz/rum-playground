(defproject rum-experiments "0.1.0-SNAPSHOT"
  :description "My first Om program!"
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.170"]
                 [rum "0.5.0" :exclusions [cljsjs/react sablono]]
                 [funcool/cats "1.1.0-SNAPSHOT"]
                 [cljsjs/react "0.14.0-1"]
                 [cljsjs/react-dom "0.14.0-1"]
                 [sablono "0.4.0"]
                 [figwheel-sidecar "0.5.0-1" :scope "test"]])

