(defproject pr-foobar "0.1.0"
  :description "Pretty print debugging macros for Clojure."
  :url "https://github.com/luxbock/pr-foobar"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :scm {:name "git", :url "https://github.com/luxbock/pr-foobar"}
  :deploy-repositories [["clojars" {:creds :gpg}]]
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [table "0.4.0"]])
