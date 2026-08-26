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

(def issues-fixture
  {:data {:customView {:issues {:nodes
    [{:identifier "W-1" :state {:name "In Review"} :title "review me" :url "u1"}
     {:identifier "W-2" :state {:name "Backlog"} :title "later" :url "u2"}
     {:identifier "W-3" :state {:name "Triage"} :title "triage me" :url "u3"}
     {:identifier "W-4" :state {:name "Brand New State"} :title "unknown" :url "u4"}]}}}})

(deftest issues-sorted-by-state
  (let [{:keys [cols rows]} (issues-view issues-fixture)]
    (is (= [:state :id :title :url] cols))
    (is (= ["W-4" "W-3" "W-2" "W-1"] (map second rows))
        "unknown states first, then Triage < Backlog < In Review")))

(def projects-fixture
  {:data {:customView {:projects {:nodes
    [{:id "p1" :status {:name "In Progress"} :name "active" :url "u1"}
     {:id "p2" :status {:name "Backlog"} :name "queued" :url "u2"}]}}}})

(deftest projects-sorted-by-status
  (is (= [["Backlog" "p2" "queued" "u2"] ["In Progress" "p1" "active" "u1"]]
         (:rows (projects-view projects-fixture)))))

(def pr-fixture
  [{:number 1 :repository {:name "repo-a"} :state "open" :isDraft false
    :title "ready" :author {:login "lukas"} :createdAt "2026-08-20T00:00:00Z"
    :commentsCount 2 :url "u1"}
   {:number 2 :repository {:name "repo-b"} :state "open" :isDraft true
    :title "wip" :author {:login "lukas"} :createdAt "2026-08-19T00:00:00Z"
    :commentsCount 0 :url "u2"}])

(deftest prs-drafts-first
  (let [rows (:rows (prs-view {:requested pr-fixture :mine []}))]
    (is (= ["draft" "review"] (map #(nth % 2) rows)))
    (is (= "2 comments" (nth (second rows) 6)))))

(deftest todo-skips-drafts-and-links
  (let [view (todo-view pr-fixture)]
    (is (= [["repo-a#1" "ready" "u1"]] (:rows view)))
    (is (= "- [[u1][repo-a#1]] ready" ((get-in view [:render :org]) (:rows view))))
    (is (= "- [repo-a#1](u1) ready" ((get-in view [:render :md]) (:rows view))))))

(defn- gh-comment [login typename at body]
  {:createdAt at :url (str "u-" at) :body body :author {:login login :__typename typename}})

(def comments-fixture
  {:data {:repository {:pullRequest
    {:comments {:nodes [(gh-comment "linear-code" "Bot" "2026-08-01T00:00:00Z" "<!-- linear-linkback -->")
                        (gh-comment "lukas" "User" "2026-08-05T00:00:00Z"
                                    "* **#333**\nThis stack of pull requests is managed by Graphite.")
                        (gh-comment "lukas" "User" "2026-08-06T00:00:00Z" "screenshots below")]}
     :reviews {:nodes [(assoc (gh-comment "pasta" "User" "2026-08-04T00:00:00Z" "does it need both?")
                              :state "APPROVED")
                       (assoc (gh-comment "pasta" "User" "2026-08-03T00:00:00Z" "")
                              :state "COMMENTED")]}
     :reviewThreads {:nodes
       [{:isResolved false :path "a.rb" :line 9 :originalLine 20
         :comments {:nodes [(gh-comment "rjoken" "User" "2026-08-02T00:00:00Z" "unique per attempt?")
                            (gh-comment "lukas" "User" "2026-08-02T01:00:00Z" "per invoice")]}}
        {:isResolved true :path "b.rb" :line nil :originalLine 29
         :comments {:nodes [(gh-comment "pasta" "User" "2026-08-07T00:00:00Z" "where from?")]}}]}}}}})

(deftest comments-hides-noise-and-keeps-threads-together
  (let [rows (:rows (comments-view comments-fixture {}))]
    (is (= [["rjoken" "inline"] ["lukas" "reply"] ["pasta" "approved"] ["lukas" "comment"]]
           (map #(vec (take 2 %)) rows))
        "bots, integration chatter, empty review envelopes and resolved threads drop out")
    (is (= "a.rb:9" (nth (first rows) 2)))))

(deftest comments-all-keeps-everything
  (let [rows (:rows (comments-view comments-fixture {:all true}))]
    (is (= ["linear-code" "rjoken" "lukas" "pasta" "lukas" "lukas" "pasta"]
           (map first rows))
        "--all brings back bots and resolved threads, but never empty review envelopes")
    (is (= "b.rb:29" (nth (last rows) 2)) "resolved threads fall back to originalLine")
    (is (= "resolved" (nth (last rows) 1)))))

(deftest comments-pr-ref-forms
  (is (= ["326" "-R" "WootingKb/wooting-mono"] (gh-pr-args "WootingKb/wooting-mono#326")))
  (is (= ["326"] (gh-pr-args 326)) "cli coerces a bare number, gh needs a string")
  (is (= ["https://github.com/o/r/pull/1"] (gh-pr-args "https://github.com/o/r/pull/1")))
  (is (nil? (gh-pr-args nil)) "no ref at all means gh picks the current branch"))

(deftest comments-truncation-detection
  (let [pr #(get-in % [:data :repository :pullRequest])]
    (is (not (truncated? (pr comments-fixture))) "a fixture within one page stays quiet")
    (is (truncated? (assoc-in (pr comments-fixture) [:reviews :pageInfo :hasNextPage] true)))
    (is (truncated? (assoc-in (pr comments-fixture)
                              [:reviewThreads :nodes 0 :comments :pageInfo :hasNextPage] true))
        "a thread whose replies overflow counts too")))

(deftest comments-rendering
  (let [view (comments-view comments-fixture {})
        rows (:rows view)]
    (is (= "rjoken  inline  a.rb:9  2026-08-02\n  unique per attempt?"
           (first (str/split (render-comments rows) #"\n\n"))))
    (is (str/starts-with? ((get-in view [:render :md]) rows)
                          "- [rjoken  inline  a.rb:9  2026-08-02](u-2026-08-02T00:00:00Z)\n  unique"))
    (is (str/starts-with? ((get-in view [:render :org]) rows)
                          "- [[u-2026-08-02T00:00:00Z][rjoken  inline  a.rb:9  2026-08-02]]\n"))))

(def notion-fixture
  {:results
   [{:properties {:Status {:status {:name "In progress"}}
                  :Name {:title [{:text {:content "middle"}}]}} :url "u1"}
    {:properties {:Status {:status {:name "Not started"}}
                  :Name {:title [{:text {:content "first"}}]}} :url "u2"}]})

(deftest notion-sorted-by-status
  (is (= [["Not started" "first" "u2"] ["In progress" "middle" "u1"]]
         (:rows (notion-view notion-fixture)))))

(deftest rendering
  (testing "org table escapes pipes"
    (is (= "| a ¦ b | c |" (render-org [["a | b" "c"]]))))
  (testing "table aligns columns"
    (is (= "aa  b\nc   dd" (render-table [["aa" "b"] ["c" "dd"]]))))
  (testing "json zips column names"
    (is (= [{:state "s" :id "i" :title "t" :url "u"}]
           (json/parse-string (render-json [:state :id :title :url] [["s" "i" "t" "u"]])
                              true))))
  (testing "empty results"
    (is (= "" (render-table [])))
    (is (= "" (render-org [])))))

(deftest end-to-end
  (when-let [bin (System/getenv "BIN")]
    (testing "no arguments prints usage and exits 2"
      (let [r (p/shell {:out :string :err :string :continue true} bin)]
        (is (= 2 (:exit r)))
        (is (str/includes? (:err r) "usage:"))))
    (testing "gh subcommands work against a stubbed gh"
      (let [dir (fs/create-temp-dir)
            stub (fs/file (fs/path dir "gh"))]
        (spit stub (str "#!/bin/sh\ncase \"$*\" in\n"
                        "  *graphql*) echo '" (json/generate-string comments-fixture) "' ;;\n"
                        "  \"pr view\"*) echo '{\"url\":\"https://github.com/o/r/pull/7\"}' ;;\n"
                        "  *) echo '" (json/generate-string pr-fixture) "' ;;\n"
                        "esac\n"))
        (fs/set-posix-file-permissions stub "rwxr-xr-x")
        (let [env {"PATH" (str dir ":" (System/getenv "PATH"))}
              run #(p/shell {:out :string :continue true :extra-env env} bin "gh" %1 %2 %3)
              prs (run "prs" "--format" "json")
              todo (run "todo" "--format" "md")
              comments (run "comments" "--format" "json")]
          (is (zero? (:exit prs)))
          (is (= 4 (count (json/parse-string (:out prs))))
              "review-requested + authored fixtures concatenated")
          (is (zero? (:exit todo)))
          (is (= "- [repo-a#1](u1) ready" (str/trim (:out todo))))
          (is (zero? (:exit comments)))
          (is (= ["rjoken" "lukas" "pasta" "lukas"]
                 (map #(get % "author") (json/parse-string (:out comments))))))))
    (testing "an unresolvable PR explains itself and keeps gh's diagnosis"
      (let [dir (fs/create-temp-dir)
            stub (fs/file (fs/path dir "gh"))]
        (spit stub "#!/bin/sh\necho 'no remotes point to a known GitHub host' >&2\nexit 1\n")
        (fs/set-posix-file-permissions stub "rwxr-xr-x")
        (let [r (p/shell {:out :string :err :string :continue true
                          :extra-env {"PATH" (str dir ":" (System/getenv "PATH"))}}
                         bin "gh" "comments")]
          (is (= 1 (:exit r)))
          (is (str/includes? (:err r) "no PR for the current branch of"))
          (is (str/includes? (:err r) "no remotes point to a known GitHub host"))
          (is (str/includes? (:err r) "number, url or owner/repo#n")))))))

(let [{:keys [fail error]} (run-tests)]
  (when (pos? (+ fail error))
    (System/exit 1)))
