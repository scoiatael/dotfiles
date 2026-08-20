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
        (spit stub (str "#!/bin/sh\necho '" (json/generate-string pr-fixture) "'\n"))
        (fs/set-posix-file-permissions stub "rwxr-xr-x")
        (let [env {"PATH" (str dir ":" (System/getenv "PATH"))}
              prs (p/shell {:out :string :continue true :extra-env env}
                           bin "gh" "prs" "--format" "json")
              todo (p/shell {:out :string :continue true :extra-env env}
                            bin "gh" "todo" "--format" "md")]
          (is (zero? (:exit prs)))
          (is (= 4 (count (json/parse-string (:out prs))))
              "review-requested + authored fixtures concatenated")
          (is (zero? (:exit todo)))
          (is (= "- [repo-a#1](u1) ready" (str/trim (:out todo)))))))))

(let [{:keys [fail error]} (run-tests)]
  (when (pos? (+ fail error))
    (System/exit 1)))
