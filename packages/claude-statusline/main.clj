;; Claude Code statusline helper. Reads the JSON from stdin and
;; outputs string to be displayed under the commandline

(require '[babashka.fs :as fs]
         '[babashka.process :as p]
         '[cheshire.core :as json]
         '[clojure.string :as str])

(def reset "\u001b[0m")
(def cyan "\u001b[36m")
(def green "\u001b[32m")
(def yellow "\u001b[33m")
(def red "\u001b[31m")
(def faint "\u001b[2m")

(def bar-width 10)

(defn git-branch [dir]
  (try
    (let [{:keys [exit out]} (p/shell {:dir dir :out :string :err :string :continue true}
                                      "git" "branch" "--show-current")]
      (when (zero? exit)
        (not-empty (str/trim out))))
    (catch Exception _ nil)))

(defn usage-color [pct]
  (cond (>= pct 90) red
        (>= pct 70) yellow
        :else green))

(defn context-bar [pct]
  (let [filled (-> pct (* bar-width) (quot 100) (min bar-width))
        color (usage-color pct)]
    (str color (str/join (repeat filled "█"))
         faint (str/join (repeat (- bar-width filled) "█")) reset)))

(defn duration [ms]
  (let [secs (quot ms 1000)]
    (format "%dm %ds" (quot secs 60) (rem secs 60))))

(defn statusline [status]
  (let [model (get-in status ["model" "display_name"])
        dir (get-in status ["workspace" "current_dir"])
        branch (git-branch dir)
        pct (long (or (get-in status ["context_window" "used_percentage"]) 0))
        cost (or (get-in status ["cost" "total_cost_usd"]) 0)
        ms (or (get-in status ["cost" "total_duration_ms"]) 0)
        location (cond-> (str "📁 " (fs/file-name dir))
                   branch (str " | 🌿 " branch))]
    (str cyan "[" model "]" reset " " location "\n"
         (context-bar pct) " " pct "% | "
         yellow (format "$%.2f" (double cost)) reset " | "
         "⏱️ " (duration ms))))

(when (= *file* (System/getProperty "babashka.file"))
  (println (statusline (-> *in* slurp json/parse-string))))
