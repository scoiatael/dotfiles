;; Run with: bb test.clj

(require '[babashka.fs :as fs]
         '[cheshire.core :as json]
         '[clojure.string :as str]
         '[clojure.test :refer [deftest is run-tests]])

(load-file (str (fs/parent (System/getProperty "babashka.file")) "/main.clj"))

(def example-json (-> "babashka.file" System/getProperty fs/parent (str "/example.json") slurp json/parse-string))

(def clean-repo {:branch "master" :commit "c54b3031" :ahead 0 :behind 0 :stashes 0
                 :staged 0 :unstaged 0 :conflicted 0 :untracked 0})

(defn plain [s]
  (str/replace s #"\u001b\[[0-9;]*m" ""))

(defn render [status & {:keys [git columns] :or {columns 100}}]
  (with-redefs [git-status (constantly git)]
    (plain (statusline status columns))))

(deftest renders-one-line-filled-to-columns
  (let [line (render example-json :git clean-repo)]
    (is (= (str " /current/working/directory ╱ master  "
                (str/join (repeat 17 "─"))
                "  Opus ╱ ██████████ 8% ╱ $0.01 ╱ 45s ")
           line))
    (is (= (- 100 ui-margin) (visible-width line)))))

(deftest keeps-a-gap-when-too-narrow
  (is (str/includes? (render example-json :columns 20) " ─ ")))

(deftest omits-vcs-outside-git
  (is (str/starts-with? (render example-json) " /current/working/directory  ─")))

(deftest parses-porcelain-v2
  (is (= {:branch "main" :commit "abcdef12" :ahead 2 :behind 1 :stashes 3
          :staged 2 :unstaged 1 :conflicted 1 :untracked 1}
         (parse-porcelain (str/join "\n" ["# branch.oid abcdef1234567890"
                                          "# branch.head main"
                                          "# branch.upstream origin/main"
                                          "# branch.ab +2 -1"
                                          "# stash 3"
                                          "1 M. N... 100644 100644 100644 a b f1"
                                          "1 MM N... 100644 100644 100644 a b f2"
                                          "u UU N... 1 1 1 1 a b c f3"
                                          "? new.txt"])))))

(deftest formats-vcs-like-p10k
  (is (= "master ⇣1⇡2 *3 ~1 +2 !1 ?1"
         (plain (vcs-segment (assoc clean-repo :ahead 2 :behind 1 :stashes 3
                                    :conflicted 1 :staged 2 :unstaged 1 :untracked 1)))))
  (is (= "@c54b3031" (plain (vcs-segment (assoc clean-repo :branch "(detached)")))))
  (is (= "feature/very…-branch-name"
         (plain (vcs-segment (assoc clean-repo :branch "feature/very-long-descriptive-branch-name"))))))

(deftest shortens-dirs-to-unique-prefix
  (is (= "alph" (unique-prefix "alpha" ["alpha" "alpine" "beta"])))
  (is (= "b" (unique-prefix "beta" ["alpha" "alpine" "beta"])))
  (is (= "~" (plain (dir-segment (System/getProperty "user.home"))))))

(deftest formats-duration-like-p10k
  (is (= "0s" (duration 0)))
  (is (= "1m 5s" (duration 65000)))
  (is (= "1h 0m 1s" (duration 3601000))))

(deftest context-bar-fills-with-usage
  (is (str/starts-with? (context-bar 42) (str (fg 76) "████" (fg 242))))
  (is (str/includes? (context-bar 95) (fg 196))))

(deftest handles-missing-usage
  (is (str/includes? (render (dissoc example-json "context_window" "cost")) "0% ╱ $0.00 ╱ 0s")))

(let [{:keys [fail error]} (run-tests)]
  (when (pos? (+ fail error))
    (System/exit 1)))
