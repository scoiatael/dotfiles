;; Claude Code PreToolUse hook for Bash, Write and Edit. Splits a command with
;; babashka.process/tokenize and denies three kinds of call: ones built from
;; parts this parser cannot account for, ones that create or replace files
;; directly in $HOME, and throwaway scripts handed to a general-purpose
;; interpreter, which belong in babashka. A file tool is checked on its path
;; alone. Subdirectories
;; stay fine either way — a permission rule cannot express that, since its `*`
;; matches separators too. The exit code is 0 either way so the hook protocol
;; sees a decision, not a hook failure.

(require '[babashka.process :as p]
         '[cheshire.core :as json]
         '[clojure.string :as str])

(def writers
  "Commands whose non-flag arguments name files they create, replace or remove."
  #{"touch" "mkdir" "cp" "mv" "ln" "tee" "install" "rsync" "dd"
    "rm" "rmdir" "truncate"})

(def interpreters
  "General-purpose interpreters. A script written for this session goes to bb
  instead; these still run whatever a project already ships."
  #{"python" "python2" "python3" "ruby" "perl" "node" "bash" "sh" "zsh"})

(def scratch-dirs
  "Where a throwaway script lands — /tmp, its macOS spellings, and the session
  scratchpad under them."
  #"^(/private)?/(tmp|var/folders)/")

(def script-suffixes
  "Extensions of a script one of the interpreters above would be handed."
  #"\.(py|rb|pl|sh|bash|zsh|js|mjs)$")

(def authored-script-suffixes
  "The subset refused at authoring time. .js is left out: an artifact's
  supporting files land in the scratchpad too, and only handing one to `node'
  says it is a script rather than a page asset."
  #"\.(py|rb|pl|sh|bash|zsh)$")

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
  "Names the construct that puts a command beyond this parser, or nil. Scanned
  on the raw string rather than on tokens: quoting decides whether a construct
  is live, and tokenize has dropped the quotes by the time tokens exist. Single
  quotes make everything literal; command substitution survives double ones."
  [s]
  (let [n (count s)]
    (loop [i 0, quote nil]
      (when (< i n)
        (let [c (.charAt s i)
              nxt (when (< (inc i) n) (.charAt s (inc i)))]
          (cond
            (= quote \')                (recur (inc i) (when-not (= c \') quote))
            (= c \\)                    (recur (+ i 2) quote)
            (and (= c \$) (= nxt \())   "command substitution"
            (= c \`)                    "backquotes"
            (= quote \")                (recur (inc i) (when-not (= c \") quote))
            (#{\' \"} c)                (recur (inc i) c)
            (and (#{\< \>} c) (= nxt \()) "process substitution"
            (and (= c \<) (= nxt \<))   "a heredoc"
            :else                       (recur (inc i) quote)))))))

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

(defn inline-code-flag?
  "True for the flags that hand an interpreter a program on the command line,
  clusters like `bash -lc' included. `-m' and a bare script path are not among
  them, so `python -m pytest' and a project's own script still run."
  [token]
  (boolean (or (#{"--eval" "--command" "--print"} token)
               (re-matches #"-[a-zA-Z]*[cep]" token))))

(defn scratch-script?
  "True for a script path under a temp directory, which is where a script
  written for this session ends up."
  [token]
  (boolean (and (re-find script-suffixes token)
                (re-find scratch-dirs token))))

(defn scratch-script-reason
  "Deny reason for authoring a throwaway script in something other than
  babashka, or nil."
  [path]
  (when (and (re-find authored-script-suffixes path)
             (re-find scratch-dirs path))
    (str path " is a scratch script; write it in babashka instead — a .clj file"
         " run with bb (see the bb-script skill)")))

(defn interpreter-reason
  "Deny reason for a throwaway script handed to an interpreter, or nil."
  [tokens]
  (some (fn [[cmd & args]]
          (let [bin (last (str/split cmd #"/"))]
            (when (and (interpreters bin) (seq args))
              (cond
                (some inline-code-flag? args)
                (str bin " runs inline code; write it as a babashka script and"
                     " run that with bb (see the bb-script skill)")

                (some scratch-script? args)
                (str bin " runs a scratch script; write it in babashka and run"
                     " it with bb (see the bb-script skill)")))))
        (segments tokens)))

(defn home-write-reason
  "Deny reason for a path a call is about to write, or nil."
  [path home]
  (let [rel (under-home path home)]
    (cond
      (= :unknown rel)
      (str "cannot tell where " path " points; write to a literal path")

      (home-child? rel)
      (str path " writes directly into $HOME; use a subdirectory"
           " or the session scratchpad"))))

(defn bash-reason [{:keys [max-chain home]} command]
  (let [tokens (p/tokenize (normalize command))]
    (or (when-let [what (opaque command)]
          (str "command uses " what ", so what it writes cannot be"
               " checked; run the steps as separate calls"))
        (when (some #{"&"} tokens)
          "command backgrounds a process; use the run_in_background option instead")
        (interpreter-reason tokens)
        (let [n (inc (count (filter chain-separators tokens)))]
          (when (> n max-chain)
            (str "command chains " n " commands (limit " max-chain
                 "); run them separately so each result is visible")))
        (some #(home-write-reason % home) (written-paths tokens)))))

(defn decide
  "Returns nil to allow the call, or a deny reason string."
  [{:keys [home] :as opts} input]
  (let [tool-input (get (try (json/parse-string input) (catch Exception _ nil))
                        "tool_input")
        command (get tool-input "command")
        path (or (get tool-input "file_path") (get tool-input "notebook_path"))]
    (cond
      (not (str/blank? command)) (bash-reason opts command)
      (not (str/blank? path))    (or (home-write-reason path home)
                                     (scratch-script-reason path)))))

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
