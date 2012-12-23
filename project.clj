(defproject euler/euler "0.1.0-SNAPSHOT" 
  :license {:name "Eclipse Public License",
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.0-RC1"]
                 [org.clojure/data.priority-map "0.0.2"]
                 [incanter "1.3.0"]]
  :profiles {:dev {:dependencies [[vimclojure/server "2.3.6"]]}}
  :url "http://github.com/rm-hull/project-euler"
  :min-lein-version "2.0.0"
  :warn-on-reflection true
  :description "A sorry attempt at solving project.net in clojure.")
