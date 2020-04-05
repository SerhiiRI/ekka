(defproject ekka "0.0.1"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies  [[seesaw "1.5.0"]
                  [org.clojure/clojure "1.10.0"]
                 ;; [org.clojure/clojure-contrib "1.2.0"]
                  [org.clojure/java.jdbc "0.7.10"]
                  [mysql/mysql-connector-java "5.1.6"]]
  :repl-options {:init-ns ekka.core})


(defproject hrtime "0.1.0-SNAPSHOT"
  :description "HRTime - pragram for emplyer registration"
  :url ""
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [seesaw "1.5.0"]
                 [org.clojars.mjdowney/excel-clj "1.2.0"]
                 [org.clojure/java.jdbc "0.7.10"]
                 [org.xerial/sqlite-jdbc "3.30.1"]
                 [mysql/mysql-connector-java "5.1.6"]
                 [autodoc "1.1.2"]]
  :jar-name "mr-jarman-hrtime-gui.jar"
  :uberjar-name "mr-jarman-hrtime-standalone.jar"
  :main hrtime.core
  ;; :dev-dependencies [[autodoc "1.1.2"]]
  :repl-options {:init-ns hrtime.core}
  :java-source-paths ["src/java-src"]
  :javac-options     ["-Xlint:unchecked"]
  :plugins [;; [risernx/lein-autodoc "1.1.2"]
            [lein-launch4j "0.1.2"]]
  ;; :autodoc {:name "hrtime", :page-title "HRTime API Documentation"}
  :launch4j-config-file "hrtime.launch4j.xml" 
  :launch4j-install-dir "C:\\Program Files (x86)\\Launch4j\\"
  :mirrors {"clojure-http" {:url "http://build.clojure.org/releases/"}})



