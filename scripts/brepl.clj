(require
 '[cljs.repl :as repl]
 '[cljs.repl.browser :as browser]
 '[cljs.repl.server :as server])

(server/dispatch-on :get
                    (fn [{:keys [path]} _ _] (.endsWith path ".css"))
                    browser/send-static)
(repl/repl* (browser/repl-env)
            {:output-dir "out"
             :serve-static true
             :optimizations :none
             :cache-analysis true
             :source-map true})
