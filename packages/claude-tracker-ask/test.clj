;; Run with: bb test.clj
;; MAIN overrides the path to main.clj; BIN (optional) enables end-to-end
;; tests against an installed binary. Both are set by default.nix's checkPhase.

(require '[babashka.fs :as fs]
         '[babashka.process :as p]
         '[cheshire.core :as json]
         '[clojure.string :as str]
         '[clojure.test :refer [deftest is run-tests testing]])

(load-file (or (System/getenv "MAIN")
               (str (fs/parent (System/getProperty "babashka.file")) "/main.clj")))

(defn bash [command] (json/generate-string {:tool_name "Bash" :tool_input {:command command}}))

(deftest asks-on-gh-issue-writes
  (is (ask? (bash "gh issue create --title x")))
  (is (ask? (bash "gh issue edit 12 --body-file f.md")))
  (testing "inside a chain or with extra spaces"
    (is (ask? (bash "cd repo && gh  issue create -t x")))))

(deftest passes-other-bash
  (is (not (ask? (bash "gh issue list"))))
  (is (not (ask? (bash "gh issue view 12"))))
  (is (not (ask? (bash "gh pr create --title x"))))
  (testing "commands a hook `if` would wrongly match"
    (is (not (ask? (bash "for f in a b; do cat /x/$f.nix | bb -e '(println 1)'; done"))))
    (is (not (ask? (bash "echo $(date)"))))))

(deftest asks-on-routed-tools
  (is (ask? (json/generate-string {:tool_name "mcp__claude_ai_Linear__save_issue"
                                   :tool_input {:title "x"}}))))

(deftest nothing-to-check-passes
  (is (not (ask? "not json")))
  (is (not (ask? (json/generate-string {:tool_name "Bash" :tool_input {}})))))

(deftest end-to-end
  (when-let [bin (System/getenv "BIN")]
    (testing "ask exits 0 with a permissionDecision"
      (let [r (p/shell {:in (bash "gh issue create") :out :string :continue true} bin)
            out (json/parse-string (:out r) true)]
        (is (zero? (:exit r)))
        (is (= "ask" (get-in out [:hookSpecificOutput :permissionDecision])))))
    (testing "pass exits 0 with no output"
      (let [r (p/shell {:in (bash "ls") :out :string :continue true} bin)]
        (is (zero? (:exit r)))
        (is (str/blank? (:out r)))))))

(let [{:keys [fail error]} (run-tests)]
  (when (pos? (+ fail error))
    (System/exit 1)))
