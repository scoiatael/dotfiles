;; Claude Code PreToolUse hook for Bash. Splits the command with
;; babashka.process/tokenize and denies two kinds of call: ones built from
;; parts this parser cannot account for, and ones that create or replace files
;; directly in $HOME (subdirectories stay fine). The exit code is 0 either way
;; so the hook protocol sees a decision, not a hook failure.

(require '[babashka.process :as p]
         '[cheshire.core :as json]
         '[clojure.string :as str])

(def writers
  "Commands whose non-flag arguments name files they create, replace or remove."
  #{"touch" "mkdir" "cp" "mv" "ln" "tee" "install" "rsync" "dd"
    "rm" "rmdir" "truncate"})

(def separators
  "Operators that end one command and start the next."
  #{"|" "||" "&&" ";"})

(def chain-separators
  "The subset that counts towards the chaining limit. A pipeline is one data
  flow, so it is left out; anything it writes is still checked below."
  #{"||" "&&" ";"})

(defn redirect? [token]
  (boolean (re-matches #"\d*>>?|>\||&>>?|>&" token)))

(defn normalize
  "Pads the operators that separate commands with spaces, so tokenize returns
  them as tokens of their own rather than glued to a word. Quoted and escaped
  occurrences are left alone, as are the `<(`/`>(` of process substitution."
  [s]
  (let [out (StringBuilder.)
        n (count s)]
    (loop [i 0, quote nil]
      (if (>= i n)
        (str out)
        (let [c (.charAt s i)
              nxt (when (< (inc i) n) (.charAt s (inc i)))
              op (when (nil? quote)
                   (case c
                     \& (cond (= nxt \>) "&>" (= nxt \&) "&&" :else "&")
                     \> (cond (= nxt \&) ">&" (= nxt \>) ">>" :else ">")
                     \| (if (= nxt \|) "||" "|")
                     \< (if (= nxt \<) "<<" "<")
                     \; ";"
                     nil))]
          (cond
            (and (nil? quote) (= c \\))
            (do (.append out c)
                (when nxt (.append out nxt))
                (recur (+ i 2) quote))

            quote
            (do (.append out c)
                (recur (inc i) (when-not (= c quote) quote)))

            (#{\' \"} c)
            (do (.append out c) (recur (inc i) c))

            ;; keep `<(`/`>(` glued so opaque still sees the substitution
            (and (#{\< \>} c) (= nxt \())
            (do (.append out c) (.append out nxt) (recur (+ i 2) quote))

            op
            (do (.append out (str " " op " "))
                (recur (+ i (count op)) nil))

            :else
            (do (.append out c) (recur (inc i) quote))))))))

(defn opaque
  "Names the construct that puts a token beyond this parser, or nil."
  [token]
  (cond
    (str/includes? token "$(")      "command substitution"
    (str/includes? token "`")       "backquotes"
    (re-find #"[<>]\(" token)       "process substitution"
    (str/starts-with? token "<<")   "a heredoc"))

(defn segments
  "Tokens grouped into the individual commands they make up."
  [tokens]
  (remove empty?
          (reduce (fn [acc t]
                    (if (separators t)
                      (conj acc [])
                      (update acc (dec (count acc)) conj t)))
                  [[]]
                  tokens)))

(defn under-home
  "The path relative to $HOME when a token points inside it, :unknown when a
  variable makes that unanswerable, nil when it points elsewhere."
  [token home]
  (let [expanded (-> token
                     (str/replace #"^\$\{?HOME\}?" home)
                     (str/replace #"^~(?=/)" home))]
    (cond
      (str/starts-with? expanded (str home "/")) (subs expanded (inc (count home)))
      (str/includes? expanded "$")               :unknown
      :else                                      nil)))

(defn home-child?
  "True for a path directly inside $HOME — ~/notes.md, but not ~/Documents/x."
  [rel]
  (and (string? rel)
       (seq rel)
       (not (str/includes? (str/replace rel #"/+$" "") "/"))))

(defn written-paths
  "Tokens sitting where a command names a file it writes to."
  [tokens]
  (concat (keep (fn [[op target]] (when (and (redirect? op) target) target))
                (partition 2 1 tokens))
          (mapcat (fn [[cmd & args]]
                    (when (writers (last (str/split cmd #"/")))
                      (remove #(str/starts-with? % "-") args)))
                  (segments tokens))))

(defn decide
  "Returns nil to allow the call, or a deny reason string."
  [{:keys [max-chain home]} input]
  (let [command (get-in (try (json/parse-string input) (catch Exception _ nil))
                        ["tool_input" "command"])]
    (when-not (str/blank? command)
      (let [tokens (p/tokenize (normalize command))]
        (or (some #(when-let [what (opaque %)]
                     (str "command uses " what ", so what it writes cannot be"
                          " checked; run the steps as separate calls"))
                  tokens)
            (when (some #{"&"} tokens)
              "command backgrounds a process; use the run_in_background option instead")
            (let [n (inc (count (filter chain-separators tokens)))]
              (when (> n max-chain)
                (str "command chains " n " commands (limit " max-chain
                     "); run them separately so each result is visible")))
            (some (fn [token]
                    (let [rel (under-home token home)]
                      (cond
                        (= :unknown rel)
                        (str "cannot tell where " token " points; write to a literal path")

                        (home-child? rel)
                        (str token " writes directly into $HOME; use a subdirectory"
                             " or the session scratchpad"))))
                  (written-paths tokens)))))))

(defn deny [reason]
  (println (json/generate-string
            {:hookSpecificOutput
             {:hookEventName "PreToolUse"
              :permissionDecision "deny"
              :permissionDecisionReason reason}})))

(defn parse-opts [args]
  (loop [opts {:max-chain 3} args args]
    (let [[a b & more] args]
      (cond
        (nil? a)             (assoc opts :home (System/getenv "HOME"))
        (= "--max-chain" a)  (recur (assoc opts :max-chain (parse-long b)) more)
        :else                (recur opts (rest args))))))

(when (= *file* (System/getProperty "babashka.file"))
  (some-> (decide (parse-opts *command-line-args*) (slurp *in*)) deny))
