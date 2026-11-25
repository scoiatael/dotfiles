;; Run with: bb test.clj
;; MAIN overrides the path to main.clj; BIN (optional) enables end-to-end
;; tests against an installed binary. Both are set by default.nix's build.

(require '[babashka.fs :as fs]
         '[babashka.process :as p]
         '[clojure.edn :as edn]
         '[clojure.string :as str]
         '[clojure.test :refer [deftest is run-tests testing]])

(load-file (or (System/getenv "MAIN")
               (str (fs/parent (System/getProperty "babashka.file")) "/main.clj")))

(defn new-repo
  "A repo with an empty commit on main and branch feat checked out."
  []
  (let [dir (str (fs/create-temp-dir))
        sh #(apply p/shell {:dir dir :out :string :err :string} %&)]
    (sh "git" "init" "-q" "-b" "main")
    (sh "git" "-c" "user.email=t@t" "-c" "user.name=t" "commit" "-q" "--allow-empty" "-m" "base")
    (sh "git" "checkout" "-q" "-b" "feat")
    dir))

(defn blob-text [repo blob]
  (:out (p/shell {:dir repo :out :string} "git" "cat-file" "-p" blob)))

(deftest seen-files-hide-until-they-change
  (let [repo (new-repo)
        changed [["A" "a.txt"] ["A" "b.txt"] ["D" "gone.txt"]]
        paths (map second changed)
        _ (spit (str repo "/a.txt") "one\n")
        _ (spit (str repo "/b.txt") "two\n")
        ids (content-ids repo paths true)
        seen (get (record-seen {} "feat" (select-keys ids ["a.txt" "gone.txt"]) "abcdef0123" "now") "feat")]
    (is (= deleted (ids "gone.txt")))
    (testing "unchanged seen files are hidden"
      (let [{:keys [shown hidden]} (classify changed seen ids false)]
        (is (= ["a.txt" "gone.txt"] (map :path hidden)))
        (is (= ["b.txt"] (map :path shown)))))
    (testing "--all shows them again, diffed against the base"
      (let [{:keys [shown hidden]} (classify changed seen ids true)]
        (is (empty? hidden))
        (is (not-any? :seen shown))))
    (testing "a changed seen file is shown against the content seen"
      (spit (str repo "/a.txt") "one\nchanged\n")
      (let [{:keys [shown]} (classify changed seen (content-ids repo paths false) false)
            a (first (filter #(= "a.txt" (:path %)) shown))]
        (is (= "one\n" (blob-text repo (get-in a [:seen :blob]))))))))

(deftest notes-checklist
  (let [section (seen-section "feat"
                              [{:path "a.txt"} {:path "b c.txt" :seen {:head "abcdef0123"}}]
                              [{:path "d.txt"}])
        notes (str "# Notes\n\nhi\n\n" section)
        ticked (str/replace notes "- [ ] `b c.txt`" "- [x] `b c.txt`")]
    (is (str/includes? section "changed since you saw it at abcdef0"))
    (is (str/includes? section "`d.txt`"))
    (is (= ["b c.txt"] (ticked-paths ticked)))
    (is (= "# Notes\n\nhi" (without-seen-section ticked)))
    (is (= "# Notes" (without-seen-section "# Notes\n")))))

(defn fake-gram
  "A gram on PATH that, on the blocking -w call, ticks a.txt in the notes file."
  []
  (let [dir (str (fs/create-temp-dir))
        f (str dir "/gram")]
    (spit f (str "#!/bin/sh\n"
                 "for a; do last=$a; done\n"
                 "case \" $* \" in *\" -w \"*) sed -i.bak 's/^- \\[ \\] `a.txt`/- [x] `a.txt`/' \"$last\";; esac\n"))
    (fs/set-posix-file-permissions f "rwxr-xr-x")
    dir))

(deftest end-to-end
  (when-let [bin (System/getenv "BIN")]
    (let [repo (new-repo)
          notes (str (fs/create-temp-dir) "/REVIEW.md")
          env {"PATH" (str (fake-gram) ":" (System/getenv "PATH"))}
          run #(apply p/shell {:dir repo :out :string :extra-env env} bin %&)]
      (spit (str repo "/a.txt") "one\n")
      (spit (str repo "/b.txt") "two\n")
      (spit notes "# Review\n")
      (testing "a ticked file is recorded as seen"
        (is (str/includes? (:out (run "--notes" notes "main")) "marked as seen\n\na.txt"))
        (is (= #{"a.txt"} (set (keys (get (edn/read-string (slurp (seen-file repo))) "feat"))))))
      (testing "the next review hides it"
        (let [out (:out (run "--dry-run" "main"))]
          (is (str/includes? out "hidden as seen: a.txt"))
          (is (not (str/includes? out "/a.txt ")))))
      (testing "once it changes, it is diffed against what was seen"
        (spit (str repo "/a.txt") "one\nchanged\n")
        (is (re-find #"/seen/a\.txt " (:out (run "--dry-run" "main")))))
      (testing "with only seen files left, nothing opens"
        (spit (str repo "/a.txt") "one\n")
        (fs/delete (str repo "/b.txt"))
        (is (str/starts-with? (:out (run "--dry-run" "main")) "Nothing new to review"))))))

(let [{:keys [fail error]} (run-tests)]
  (when (pos? (+ fail error))
    (System/exit 1)))
