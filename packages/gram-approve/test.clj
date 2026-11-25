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

(def store (str (fs/create-temp-dir) "/approvals"))

(alter-var-root #'approvals-file (constantly (constantly store)))

(def now (java.time.Instant/now))

(def preview
  "Intro text is ignored.

## [x] Fix login
Users get logged out.

## Steps
1. open it

## [ ] Drop the cache
Not yet.
")

(defn bash [command cwd]
  {"tool_name" "Bash" "tool_input" {"command" command} "cwd" cwd})

(defn linear [input]
  {"tool_name" "mcp__claude_ai_Linear__save_issue" "tool_input" input})

(deftest parses-sections
  (let [[a b] (sections preview)]
    (is (= {:ticked? true :title "Fix login" :body "Users get logged out.\n\n## Steps\n1. open it"} a))
    (is (= {:ticked? false :title "Drop the cache" :body "Not yet."} b)))
  (is (not (str/includes? (untick preview) "[x]"))))

(deftest only-ticked-items-pass
  (fs/delete-if-exists store)
  (record! (map text-hash (mapcat item-texts (filter :ticked? (sections preview)))) now)
  (testing "Linear"
    (is (nil? (hook-reason (linear {"title" "Fix login" "description" "Users get logged out.\n\n## Steps\n1. open it\n"}) now)))
    (is (str/includes? (hook-reason (linear {"title" "Drop the cache" "description" "Not yet."}) now)
                       "not approved in Gram: \"Drop the cache\""))
    (is (some? (hook-reason (linear {"title" "Fix login" "description" "Edited after approval"}) now)))
    (is (some? (hook-reason (linear {"id" "X-1" "patch" "whatever"}) now))))
  (testing "Github"
    (let [dir (str (fs/create-temp-dir))]
      (spit (str dir "/b.md") "Users get logged out.\n\n## Steps\n1. open it")
      (spit (str dir "/other.md") "Unreviewed text")
      (is (nil? (hook-reason (bash "gh issue create -t 'Fix login' -F b.md" dir) now)))
      (is (nil? (hook-reason (bash "gh pr comment 5 --body-file=b.md" dir) now)))
      (is (some? (hook-reason (bash "gh pr comment 5 -F other.md" dir) now)))
      (is (some? (hook-reason (bash "gh issue create --title=Sneaky -F b.md" dir) now)))
      (is (some? (hook-reason (bash "gh pr comment 5 -F missing.md" dir) now)))))
  (testing "writes without text pass"
    (is (nil? (hook-reason (linear {"id" "X-1" "state" "Done"}) now)))
    (is (nil? (hook-reason (bash "gh pr review 5 --approve" nil) now)))
    (is (nil? (hook-reason (bash "gh issue edit 3 --add-label bug" nil) now))))
  (testing "other tools and commands pass"
    (is (nil? (hook-reason (bash "ls" nil) now)))
    (is (nil? (hook-reason {"tool_name" "mcp__claude_ai_Linear__get_issue" "tool_input" {"title" "x"}} now)))))

(deftest approvals-expire
  (fs/delete-if-exists store)
  (record! [(text-hash "Old")] (.minus now (.plusSeconds approval-ttl 1)))
  (record! [(text-hash "New")] now)
  (is (some? (hook-reason (linear {"title" "Old"}) now)))
  (is (nil? (hook-reason (linear {"title" "New"}) now)))
  (is (not (str/includes? (slurp store) (text-hash "Old")))))

(deftest end-to-end
  (when-let [bin (System/getenv "BIN")]
    (let [env {"GRAM_APPROVALS" (str (fs/create-temp-dir) "/approvals")}]
      (testing "an unapproved write is denied"
        (let [r (p/shell {:in (json/generate-string (linear {"title" "x"})) :out :string :extra-env env} bin "--hook")]
          (is (= "deny" (get-in (json/parse-string (:out r) true) [:hookSpecificOutput :permissionDecision])))))
      (testing "anything else passes with no output"
        (let [r (p/shell {:in (json/generate-string (bash "ls" nil)) :out :string :extra-env env} bin "--hook")]
          (is (str/blank? (:out r))))))))

(let [{:keys [fail error]} (run-tests)]
  (when (pos? (+ fail error))
    (System/exit 1)))
