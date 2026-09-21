;; Run with: bb test.clj
;; MAIN overrides the path to main.clj; BIN (optional) enables end-to-end
;; tests against an installed binary. Both are set by default.nix.

(require '[babashka.fs :as fs]
         '[babashka.process :as p]
         '[cheshire.core :as json]
         '[clojure.string :as str]
         '[clojure.test :refer [deftest is run-tests testing]])

(load-file (or (System/getenv "MAIN")
               (str (fs/parent (System/getProperty "babashka.file")) "/main.clj")))

(def home "/home/tester")
(def opts {:max-chain 3 :home home})

(defn hook [command] (json/generate-string {:tool_input {:command command}}))

(defn reason [command] (decide opts (hook command)))

(deftest allows-ordinary-commands
  (is (nil? (reason "ls -la")))
  (is (nil? (reason "yarn test path/to/file.test.ts")))
  (is (nil? (reason "gh run view 123 --log | grep error | tail -5")))
  (is (nil? (reason "cd /tmp/work && nix build .#serverImage"))))

(deftest denies-writes-directly-in-home
  (is (str/includes? (reason "echo x > ~/notes.md") "directly into $HOME"))
  (is (str/includes? (reason "printf x >> $HOME/f") "directly into $HOME"))
  (is (some? (reason (str "touch " home "/scratch.txt"))))
  (is (some? (reason "mkdir ~/newdir")))
  (is (some? (reason "cp a.txt ~/b.txt")))
  (is (some? (reason "mv ~/Downloads/a ~/b")))
  (is (some? (reason "rm ~/leftover.json")))
  (is (some? (reason "tee ~/out < in"))))

(deftest allows-writes-deeper-in-home
  (is (nil? (reason "echo x > ~/Documents/notes.md")))
  (is (nil? (reason "mkdir -p ~/Documents/worktrees/repo.branch")))
  (is (nil? (reason (str "cp a " home "/dotfiles/config/x"))))
  (testing "reading a direct child is not a write"
    (is (nil? (reason "cat ~/.zshrc")))
    (is (nil? (reason "ls ~/Documents")))
    (is (nil? (reason "cd ~")))))

(deftest denies-writes-elsewhere-only-when-in-home
  (is (nil? (reason "echo x > /tmp/out")))
  (is (nil? (reason "cat foo > /dev/null"))))

(deftest denies-unresolvable-write-targets
  (is (str/includes? (reason "echo x > $OUT") "cannot tell where"))
  (is (some? (reason "cp a \"$DEST\""))))

(deftest denies-constructs-it-cannot-parse
  (is (str/includes? (reason "echo $(date) > /tmp/f") "command substitution"))
  (is (str/includes? (reason "echo `date`") "backquotes"))
  (is (str/includes? (reason "diff <(a) <(b)") "process substitution"))
  (is (str/includes? (reason "cat <<EOF") "heredoc"))
  (is (str/includes? (reason "long-task &") "backgrounds"))
  (testing "single quotes make a construct literal, so it stays allowed"
    (is (nil? (reason "q list '`CloseTime` BETWEEN \"a\" AND \"b\"' | jq -r .x")))
    (is (nil? (reason "rg '\\$\\(x\\)' src")))
    (is (nil? (reason "echo 'cost: $(5)'")))
    (is (nil? (reason "psql -c 'select `a` from t'"))))
  (testing "double quotes do not, since the shell still expands there"
    (is (str/includes? (reason "echo \"$(date)\"") "command substitution"))
    (is (str/includes? (reason "echo \"`date`\"") "backquotes"))))

(deftest sends-throwaway-scripts-to-babashka
  (testing "inline code goes to bb"
    (is (str/includes? (reason "python -c 'print(1)'") "bb"))
    (is (some? (reason "python3 -c 'print(1)'")))
    (is (some? (reason "ruby -e 'puts 1'")))
    (is (some? (reason "perl -e 'print 1'")))
    (is (some? (reason "node -e 'console.log(1)'")))
    (is (some? (reason "bash -lc 'for f in *; do echo $f; done'")))
    (is (some? (reason "sh -c 'echo hi'"))))
  (testing "a script written for this session goes to bb"
    (is (some? (reason "python /tmp/claude-501/x/scratchpad/parse.py")))
    (is (some? (reason "bash /private/tmp/claude-501/x/run.sh")))
    (is (some? (reason "node /tmp/probe.mjs"))))
  (testing "but a project's own tooling still runs"
    (is (nil? (reason "python -m pytest tests/")))
    (is (nil? (reason "python manage.py migrate")))
    (is (nil? (reason "node build.js --watch")))
    (is (nil? (reason "bash scripts/deploy.sh")))
    (is (nil? (reason "ruby bin/rails console")))
    (is (nil? (reason "python3")))
    (is (nil? (reason "bb -e '(println 1)'")))
    (is (nil? (reason "psql -c 'select 1'")))))

(deftest limits-command-chaining
  (is (nil? (reason "a && b && c")))
  (is (str/includes? (reason "a && b && c && d") "chains 4 commands"))
  (is (some? (reason "a; b; c; d")))
  (testing "pipelines are one data flow, not a chain"
    (is (nil? (reason "a | b | c | d | e")))
    (is (nil? (reason "gh run view 1 --log | grep -a error | sort | uniq -c | tail -5")))
    (testing "but a pipeline that writes into $HOME is still caught"
      (is (some? (reason "a | b | c | d | tee ~/out")))))
  (testing "the limit is configurable"
    (is (some? (decide (assoc opts :max-chain 2) (hook "a && b && c"))))
    (is (nil? (decide (assoc opts :max-chain 9) (hook "a && b && c && d && e"))))))

(deftest operators-need-no-surrounding-space
  (is (str/includes? (reason "a; b; c; d; e") "chains 5 commands"))
  (is (some? (reason "echo x>~/glued.txt")))
  (is (some? (reason "true&&touch ~/x")))
  (testing "shell forms that only look like operators"
    (is (nil? (reason "yarn build 2>&1 | tail -5")))
    (is (nil? (reason "find . -name '*.tmp' -exec rm {} \\;")))
    (is (nil? (reason "rg 'a;b' src")))
    (is (nil? (reason "echo 'a && b'")))))

(deftest checks-the-file-tools-by-path
  (let [write (fn [path] (decide opts (json/generate-string
                                       {:tool_name "Write" :tool_input {:file_path path}})))]
    (testing "a permission rule cannot express direct-child-only, so this does"
      (is (some? (write (str home "/stray.md"))))
      (is (some? (write "~/stray.md")))
      (is (nil? (write (str home "/Documents/server/src/index.ts"))))
      (is (nil? (write (str home "/.claude/projects/x/memory/note.md"))))
      (is (nil? (write "/tmp/scratch/note.md")))))
  (testing "a scratch script is refused at authoring time, not just at run time"
    (let [write (fn [path] (decide opts (json/generate-string
                                         {:tool_name "Write" :tool_input {:file_path path}})))]
      (is (str/includes? (write "/tmp/claude-501/x/scratchpad/parse.py") "babashka"))
      (is (some? (write "/private/tmp/claude-501/x/scratchpad/run.sh")))
      (is (some? (write "/tmp/probe.rb")))
      (testing "babashka itself is the point, and project scripts are untouched"
        (is (nil? (write "/tmp/claude-501/x/scratchpad/parse.clj")))
        (is (nil? (write (str home "/dotfiles/scripts/deploy.sh"))))
        (is (nil? (write (str home "/Documents/server/manage.py")))))
      (testing "an artifact's supporting files land in the scratchpad too"
        (is (nil? (write "/tmp/claude-501/x/scratchpad/report.html")))
        (is (nil? (write "/tmp/claude-501/x/scratchpad/app.js"))))))
  (testing "notebooks are named differently"
    (is (some? (decide opts (json/generate-string
                             {:tool_name "NotebookEdit"
                              :tool_input {:notebook_path (str home "/x.ipynb")}}))))))

(deftest nothing-to-check-allows
  (is (nil? (decide opts (json/generate-string {:tool_input {}}))))
  (is (nil? (decide opts (hook ""))))
  (is (nil? (decide opts "not json"))))

(deftest end-to-end
  (when-let [bin (System/getenv "BIN")]
    (testing "allow exits 0 with no output"
      (let [r (p/shell {:in (hook "ls -la") :out :string :continue true} bin)]
        (is (zero? (:exit r)))
        (is (str/blank? (:out r)))))
    (testing "deny exits 0 with a permissionDecision"
      (let [r (p/shell {:in (hook "touch ~/stray.txt") :out :string :continue true} bin)
            out (json/parse-string (:out r) true)]
        (is (zero? (:exit r)))
        (is (= "deny" (get-in out [:hookSpecificOutput :permissionDecision])))
        (is (str/includes? (get-in out [:hookSpecificOutput :permissionDecisionReason])
                           "$HOME"))))
    (testing "--max-chain is honoured"
      (let [r (p/shell {:in (hook "a && b && c") :out :string :continue true}
                       bin "--max-chain" "2")]
        (is (str/includes? (:out r) "chains 3 commands"))))))

(let [{:keys [fail error]} (run-tests)]
  (when (pos? (+ fail error))
    (System/exit 1)))
