;; Claude Code statusline helper. Reads the JSON from stdin and
;; outputs string to be displayed under the commandline

(require '[cheshire.core :as json]
         '[clojure.string :as str])


(defn statusline [status]
  (let [model (get-in status ["model" "display_name"])]
    (format "model: %s\n" model)))

(when (= *file* (System/getProperty "babashka.file"))
  (println (statusline (-> *in* slurp json/parse-string))))
