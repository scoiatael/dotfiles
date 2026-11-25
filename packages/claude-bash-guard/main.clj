;; Claude Code PreToolUse hook for Bash, Write and Edit. Splits a command with
;; babashka.process/tokenize and denies seven kinds of call: ones built from
;; parts this parser cannot account for, ones that create or replace files
;; directly in $HOME, throwaway scripts handed to a general-purpose
;; interpreter, which belong in babashka, searches rooted at / or $HOME,
;; git commits outside a linked worktree under ~/Documents/worktrees,
;; Github writes in a form gram-approve cannot match against a preview, and
;; anything touching gram-approve's approvals file.
;; A file tool is checked on its path alone. Subdirectories
;; stay fine either way — a permission rule cannot express that, since its `*`
;; matches separators too. The exit code is 0 either way so the hook protocol
;; sees a decision, not a hook failure.

(require '[babashka.fs :as fs]
         '[babashka.process :as p]
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

(def searchers
  "Commands that walk a directory tree looking for files or content."
  #{"find" "fd" "fdfind" "rg" "grep" "ag" "ack"})

(def pattern-first-searchers
  "The subset whose first non-flag argument is the pattern, not a path."
  #{"fd" "fdfind" "rg" "grep" "ag" "ack"})

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

(defn search-root?
  "True for a token naming / or $HOME itself, however it is spelled."
  [token home]
  (let [expanded (-> token
                     (str/replace #"^\$\{?HOME\}?$" home)
                     (str/replace #"^\$\{?HOME\}?(?=/)" home)
                     (str/replace #"^~$" home)
                     (str/replace #"^~(?=/)" home)
                     (str/replace #"(?<=.)/+$" ""))]
    (contains? #{"/" home} expanded)))

(defn search-root-reason
  "Deny reason for a search rooted at / or $HOME, or nil. Such a walk hits
  macOS's permission-protected directories and buries the hits in errors."
  [tokens home]
  (some (fn [[cmd & args]]
          (let [bin (last (str/split cmd #"/"))
                args (remove #(str/starts-with? % "-") args)
                paths (if (pattern-first-searchers bin) (rest args) args)]
            (when (and (searchers bin) (some #(search-root? % home) paths))
              (str bin " searches all of / or $HOME; pick one directory to"
                   " search — `ls` the top level first (see the"
                   " filesystem-search skill)"))))
        (segments tokens)))

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

(def git-valued-options
  "Global git options that consume the next token."
  #{"-C" "-c" "--git-dir" "--work-tree" "--namespace" "--config-env"})

(defn git-subcommand
  "The subcommand of a git invocation and the -C directories before it, or nil."
  [[cmd & args]]
  (when (= "git" (last (str/split cmd #"/")))
    (loop [[a b & more :as args] args, dirs []]
      (cond
        (nil? a)                nil
        (= "-C" a)              (recur more (conj dirs b))
        (git-valued-options a)  (recur more dirs)
        (str/starts-with? a "-") (recur (rest args) dirs)
        :else                   {:sub a :dirs dirs}))))

(defn resolve-dir
  "The absolute directory a token names from base, or nil when a variable or
  an unknown base leaves it unanswerable."
  [base token home]
  (let [expanded (-> token
                     (str/replace #"^\$\{?HOME\}?(?=/|$)" home)
                     (str/replace #"^~(?=/|$)" home))]
    (cond
      (str/includes? expanded "$")      nil
      (fs/absolute? expanded)           (str (fs/normalize expanded))
      base                              (str (fs/normalize (fs/path base expanded))))))

(defn commit-dirs
  "The directory each `git commit` in the command runs in, following `cd` and
  -C. nil stands for one that cannot be resolved."
  [tokens cwd home]
  (:dirs
   (reduce (fn [{:keys [cwd] :as acc} [cmd & args :as segment]]
             (let [git (git-subcommand segment)]
               (cond
                 (= "cd" cmd)
                 (assoc acc :cwd (if-let [to (first args)] (resolve-dir cwd to home) home))

                 (= "commit" (:sub git))
                 (update acc :dirs conj (reduce #(when %1 (resolve-dir %1 %2 home))
                                                cwd (:dirs git)))

                 :else acc)))
           {:cwd cwd :dirs []}
           (segments tokens))))

(defn linked-worktree?
  "True for a directory inside a linked worktree under ~/Documents/worktrees.
  Those have a .git file pointing at their clone; the clone itself, which is
  the user's checkout, has a .git directory."
  [dir home]
  (let [root (str home "/Documents/worktrees/")]
    (boolean
     (when (and dir (str/starts-with? (str dir "/") root))
       (let [name (first (str/split (subs (str dir "/") (count root)) #"/"))]
         (and (seq name) (fs/regular-file? (str root name "/.git"))))))))

(defn commit-reason
  "Deny reason for a git commit outside a linked worktree, or nil."
  [tokens cwd home]
  (when (some #(not (linked-worktree? % home)) (commit-dirs tokens cwd home))
    (str "git commit is the user's outside a linked worktree under"
         " ~/Documents/worktrees; leave the changes in the working tree, or"
         " print the command for them")))

(def gh-writes
  "gh subcommands that post text; gram-approve checks them against a preview."
  {"issue" #{"create" "edit" "comment"}
   "pr"    #{"create" "edit" "comment" "review"}})

(defn gh-write? [[cmd group sub]]
  (boolean (and (= "gh" (last (str/split cmd #"/")))
                (contains? (get gh-writes group) sub))))

(defn flag-value
  "The value of the first of names in args, given as `--name v` or `--name=v`."
  [args names]
  (some (fn [[a b]]
          (or (when (names a) b)
              (some #(when (str/starts-with? a (str % "=")) (subs a (inc (count %)))) names)))
        (partition-all 2 1 args)))

(defn gh-api-write?
  "True for a `gh api` call that changes something. Fields make gh default to
  POST, and a graphql call always POSTs, so only a mutation writes there; a
  query read from a file could be one, so it counts as a write."
  [[cmd sub & args]]
  (when (and (= "gh" (last (str/split cmd #"/"))) (= "api" sub))
    (let [method (or (flag-value args #{"-X" "--method"})
                     (some #(second (re-matches #"-X(\w+)" %)) args))
          fields? (some #(re-matches #"-[fF].*|--(raw-)?field(=.*)?|--input(=.*)?" %) args)]
      (cond
        method                    (not= "GET" (str/upper-case method))
        (some #{"graphql"} args)  (boolean (some #(re-find #"\bmutation\b|^query=@" %) args))
        :else                     (boolean fields?)))))

(defn gh-reason
  "Deny reason for a Github write gram-approve could not match to a preview, or
  nil. The one form allowed is a lone `gh issue|pr <write>` with --body-file."
  [tokens]
  (let [segs (segments tokens)]
    (some (fn [[_ group sub & args :as seg]]
            (cond
              (gh-api-write? seg)
              (str "gh api writes cannot be reviewed; use gh issue/pr"
                   " create|edit|comment|review with --body-file")

              (not (gh-write? seg)) nil

              (> (count segs) 1)
              (str "run gh " group " " sub " on its own, not chained or piped,"
                   " so gram-approve can check exactly what it sends")

              (or (some #(re-matches #"-b|--body(=.*)?" %) args)
                  (= "-" (flag-value args #{"-F" "--body-file"})))
              (str "gh " group " " sub " takes its text inline or from stdin;"
                   " write it to a markdown file, approve it with gram-approve,"
                   " and pass --body-file")))
          segs)))

(defn approvals-reason
  "Deny reason for a call that names gram-approve's approvals file, or nil.
  Only a tick in Gram may add to it."
  [strings]
  (when (some #(str/includes? % "gram-approvals") strings)
    "the approvals file is written only by gram-approve, from ticks in Gram"))

(defn bash-reason [{:keys [max-chain home cwd]} command]
  (let [tokens (p/tokenize (normalize command))]
    (or (when-let [what (opaque command)]
          (str "command uses " what ", so what it writes cannot be"
               " checked; run the steps as separate calls"))
        (when (some #{"&"} tokens)
          "command backgrounds a process; use the run_in_background option instead")
        (interpreter-reason tokens)
        (search-root-reason tokens home)
        (commit-reason tokens cwd home)
        (gh-reason tokens)
        (approvals-reason tokens)
        (let [n (inc (count (filter chain-separators tokens)))]
          (when (> n max-chain)
            (str "command chains " n " commands (limit " max-chain
                 "); run them separately so each result is visible")))
        (some #(home-write-reason % home) (written-paths tokens)))))

(defn decide
  "Returns nil to allow the call, or a deny reason string."
  [{:keys [home] :as opts} input]
  (let [event (try (json/parse-string input) (catch Exception _ nil))
        tool-input (get event "tool_input")
        command (get tool-input "command")
        path (or (get tool-input "file_path") (get tool-input "notebook_path"))]
    (cond
      (not (str/blank? command)) (bash-reason (assoc opts :cwd (get event "cwd")) command)
      (not (str/blank? path))    (or (approvals-reason [path])
                                     (home-write-reason path home)
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
