#?(:clj ;; Only load this in Clojure not Clojurescript
   (do
     ;; See this post for more details
     ;; https://clojure-goes-fast.com/blog/system-wide-user-clj/
     (in-ns 'user)

     (defn heap []
       (let [u     (.getHeapMemoryUsage (java.lang.management.ManagementFactory/getMemoryMXBean))
             used  (/ (.getUsed u) 1e6)
             total (/ (.getMax u) 1e6)]
         (format "Used: %.0f/%.0f MB (%.0f%%)" used total (/ used total 0.01))))

     (defmacro bench [expr & opts]
       (println "Starting bench ...\n")
       (require '[criterium.core :as crit])
       `(crit/quick-bench ~expr ~@opts))

     (println "Loaded system-wide user.clj!")))

