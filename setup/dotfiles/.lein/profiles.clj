{:user {:dependencies [[compliment "0.3.8"]
                       [org.clojure/tools.namespace "0.3.0"]
                       [pjstadig/humane-test-output "0.9.0"]
                       [clj-repl-enrichment "58e1548f60683475649e82951ae698d11ea415ed"]]
        :plugins      [[reifyhealth/lein-git-down "0.3.6"]]
        :middleware   [lein-git-down.plugin/inject-properties]
        :repositories [["public-github" {:url "git://github.com/"}]]
        :git-down     {clj-repl-enrichment {:coordinates andersmurphy/clj-repl-enrichment}}}}
