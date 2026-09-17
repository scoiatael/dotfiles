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
  (is (str/includes? (reason "long-task &") "backgrounds")))

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
