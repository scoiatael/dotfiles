;; Claude Code PreToolUse hook. Asks for approval before a tracker item is
;; written: a Bash command running `gh issue create|edit`, or any other tool
;; the hook matcher routed here (the Linear MCP save_* tools). Matching the
;; command text here is strict, unlike a hook's `if`, which also fires on any
;; command with $VAR or one Claude Code can't split.

(require '[cheshire.core :as json])

(def gh-issue-write #"\bgh\s+issue\s+(create|edit)\b")

(def reason
  "Writes a tracker item. Approve only if you reviewed its markdown preview in Gram.")

(defn ask? [input]
  (let [{:strs [tool_name tool_input]} (try (json/parse-string input) (catch Exception _ nil))]
    (if (= "Bash" tool_name)
      (boolean (re-find gh-issue-write (get tool_input "command" "")))
      (some? tool_name))))

(defn ask []
  (println (json/generate-string
            {:hookSpecificOutput
             {:hookEventName "PreToolUse"
              :permissionDecision "ask"
              :permissionDecisionReason reason}})))

(when (= *file* (System/getProperty "babashka.file"))
  (when (ask? (slurp *in*)) (ask)))
