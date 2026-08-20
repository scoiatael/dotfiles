;; Claude Code PreToolUse hook helper. Reads the hook JSON from stdin and
;; allows the call only if tool_input.url matches one of the origins given as
;; arguments. Entries without a scheme (e.g. "localhost:3000") match both
;; http and https. A non-matching URL is denied by printing a PreToolUse
;; permissionDecision on stdout; the exit code is 0 either way so the hook
;; protocol sees a decision, not a hook failure.

(require '[cheshire.core :as json]
         '[clojure.string :as str])

(def default-port {"http" 80 "https" 443})

(defn parse-origin [s]
  (let [u (java.net.URI. s)]
    {:scheme (some-> (.getScheme u) .toLowerCase)
     :host   (some-> (.getHost u) .toLowerCase)
     :port   (when (pos? (.getPort u)) (.getPort u))}))

(defn parse-target [url]
  (let [t (parse-origin url)]
    (when (:host t)
      (update t :port #(or % (default-port (:scheme t)))))))

(defn matches? [entry {:keys [scheme host port]}]
  (let [pinned? (str/includes? entry "://")
        ;; java.net.URI reads "localhost:3000" as scheme "localhost"
        e (parse-origin (if pinned? entry (str "http://" entry)))]
    (and (= (:host e) host)
         (if pinned?
           (and (= (:scheme e) scheme)
                (= (or (:port e) (default-port (:scheme e))) port))
           (and (contains? default-port scheme)
                (= (or (:port e) (default-port scheme)) port))))))

(defn decide
  "Returns nil to allow the call, or a deny reason string.
  A missing or malformed url means \"nothing to check\"."
  [allowed input]
  (let [url (get-in (try (json/parse-string input) (catch Exception _ nil))
                    ["tool_input" "url"])]
    (when-not (empty? url)
      (if-let [target (try (parse-target url) (catch Exception _ nil))]
        (when-not (some #(try (matches? % target) (catch Exception _ false)) allowed)
          (str "URL not in allowlist (" (str/join ", " allowed)
               "); refusing to navigate to " url))
        (str "could not parse URL " (pr-str url) "; refusing to navigate")))))

(defn deny [reason]
  (println (json/generate-string
            {:hookSpecificOutput
             {:hookEventName "PreToolUse"
              :permissionDecision "deny"
              :permissionDecisionReason reason}})))

(when (= *file* (System/getProperty "babashka.file"))
  (when (empty? *command-line-args*)
    (binding [*out* *err*]
      (println "usage: claude-url-allowlist <origin>... (e.g. localhost:3000 https://example.com)"))
    (System/exit 2))
  (some-> (decide *command-line-args* (slurp *in*)) deny))
