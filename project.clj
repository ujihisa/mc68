(defproject mc68 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [twitter-api "0.6.13"]
                 [swank-clojure/swank-clojure "1.4.2"]]
  :dev-dependencies [[org.bukkit/bukkit "1.4.5-R0.3-SNAPSHOT"]]
  :repositories {"org.bukkit"
                 "http://repo.bukkit.org/service/local/repositories/snapshots/content/"}
  :java-source-paths ["javasrc"])
