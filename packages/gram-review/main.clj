;; Hand a branch to the user for review in Gram, and report back what they did.
;;
;;   ,gram-review --notes REVIEW.md          diff against the merge-base with origin/HEAD
;;   ,gram-review --notes REVIEW.md main     diff against another base
;;   ,gram-review --notes REVIEW.md --pr 123 review a Github PR in its worktree, against its base branch
;;   ,gram-review --notes REVIEW.md --pr URL a Github PR URL, from any directory
;;   ,gram-review --all ...                  also show files the user marked as seen
;;   ,gram-review --dry-run ...              print the gram command, open nothing
;;
;; Without --notes, the window gets an empty notes file for the user to write in.
;;
;; Opens one Gram window with the repo, the notes file and a base-vs-current
;; diff per changed file (committed, uncommitted and untracked alike). The
;; right side of each diff is the real file, so edits there land in the
;; worktree; the base side is a read-only copy. Blocks until the window
;; closes, then prints the notes and the files the user edited.
;;
;; The notes file gets a "Mark as seen" checklist of the files shown. A ticked
;; file's content is stored as a git blob, per branch, in the repo's common git
;; dir. Later reviews of that branch hide the file while its content is
;; unchanged, and diff it against the seen content once it changes.

(require '[babashka.fs :as fs]
         '[babashka.process :as p]
         '[cheshire.core :as json]
         '[clojure.edn :as edn]
         '[clojure.string :as str])

(defn- die [& msg]
  (binding [*out* *err*] (apply println msg))
  (System/exit 1))

(defn- git [dir & args]
  (let [r (apply p/shell {:dir dir :out :string :err :string :continue true} "git" args)]
    (when (zero? (:exit r)) (str/trim-newline (:out r)))))

(defn- toplevel []
  (or (git "." "rev-parse" "--show-toplevel") (die "not inside a git repo")))

(defn- default-base [root]
  (or (some #(git root "merge-base" "HEAD" %)
            ["origin/HEAD" "origin/main" "origin/master" "main" "master"])
      (die "no base found; pass one, e.g. ,gram-review --notes REVIEW.md main")))

(defn- worktree-on [root branch]
  (->> (str/split (git root "worktree" "list" "--porcelain") #"\n\n")
       (keep (fn [entry]
               (let [path (second (re-find #"(?m)^worktree (.+)$" entry))]
                 (when (str/includes? (str entry "\n") (str "\nbranch refs/heads/" branch "\n"))
                   path))))
       first))

(def worktrees-dir (fs/path (fs/home) "Documents" "worktrees"))

(defn- pr-repo
  "[clone pr-number] for a PR number (repo of the current checkout) or a Github PR URL."
  [pr]
  (if-let [[_ repo n] (re-find #"github\.com/[^/]+/([^/]+)/pull/(\d+)" pr)]
    (let [clone (fs/path worktrees-dir repo)]
      (when-not (fs/exists? clone) (die "no clone of" repo "at" (str clone)))
      [(str clone) n])
    (if (re-matches #"\d+" pr)
      [(toplevel) pr]
      (die "--pr takes a number or a Github PR URL; resolve a Linear review URL to its PR first"))))

(defn- pr-checkout
  "[worktree base] for a Github PR, creating the worktree with wt when it has none yet."
  [pr]
  (let [[root pr] (pr-repo pr)
        r (p/shell {:dir root :out :string :err :string :continue true}
                   "gh" "pr" "view" pr "--json" "headRefName,baseRefName")
        _ (when-not (zero? (:exit r)) (die "gh pr view" pr "failed:" (:err r)))
        {:keys [headRefName baseRefName]} (json/parse-string (:out r) true)
        tree (or (worktree-on root headRefName)
                 (do (p/shell {:dir root :out *err*} "wt" "switch" (str "pr:" pr) "--no-cd" "-y")
                     (worktree-on root headRefName))
                 (die "wt switch pr:" pr "made no worktree on branch" headRefName))]
    (git tree "fetch" "--quiet" "origin" baseRefName)
    [tree (or (git tree "merge-base" "HEAD" (str "origin/" baseRefName))
              (die "no merge-base with origin/" baseRefName))]))

(defn- changes
  "[status path] for every file that differs from base, untracked included."
  [root base]
  (concat (->> (str/split (git root "diff" "--name-status" "--no-renames" "-z" base) #"\x00")
               (remove str/blank?)
               (partition 2)
               (map (fn [[status path]] [(subs status 0 1) path])))
          (->> (str/split (git root "ls-files" "-z" "--others" "--exclude-standard") #"\x00")
               (remove str/blank?)
               (map (fn [path] ["A" path])))))

(defn- empty-file [dir path]
  (let [f (fs/path dir path)]
    (fs/create-dirs (fs/parent f))
    (spit (str f) "")
    (str f)))

(defn- read-only-copy
  "path's content in the git object blob-ish (e.g. base:path, or a blob id) under dir, empty for nil."
  [root dir blob-ish path]
  (let [f (empty-file dir path)]
    (when blob-ish
      (p/shell {:dir root :out (fs/file f)} "git" "cat-file" "-p" blob-ish))
    (fs/set-posix-file-permissions f "r--r--r--")
    f))

;; Seen state

(def deleted "deleted")

(defn seen-file [root]
  (str (fs/path (git root "rev-parse" "--path-format=absolute" "--git-common-dir")
                "gram-review-seen.edn")))

(defn load-seen [f]
  (if (fs/exists? f) (edn/read-string (slurp f)) {}))

(defn content-ids
  "{path blob-id} for the current content of paths, `deleted` for a missing file.
  With write?, the blobs are also stored, so the content can be shown later."
  [root paths write?]
  (let [present (filter #(fs/exists? (fs/path root %)) paths)
        ids (when (seq present)
              (str/split-lines
               (apply git root "hash-object" (concat (when write? ["-w"]) ["--"] present))))]
    (merge (zipmap paths (repeat deleted))
           (zipmap present ids))))

(defn classify
  "Splits [status path] changes into :shown and :hidden, given the branch's seen
  entries and the current content ids. A shown file carries the :seen entry it
  changed from, if any."
  [changed seen ids all?]
  (let [unchanged? #(= (:blob (get seen %)) (ids %))]
    (group-by #(if (:unchanged? %) :hidden :shown)
              (for [[status path] changed
                    :let [unchanged (unchanged? path)]]
                (cond-> {:status status :path path}
                  (and unchanged (not all?)) (assoc :unchanged? true)
                  (and (get seen path) (not unchanged)) (assoc :seen (get seen path)))))))

(def seen-heading "## Mark as seen")

(defn seen-section [branch shown hidden]
  (str seen-heading "\n"
       "<!-- Tick a file to hide it from later reviews of " branch " until it changes. -->\n"
       (str/join (for [{:keys [path seen]} shown]
                   (str "- [ ] `" path "`"
                        (when seen (str " — changed since you saw it at "
                                        (subs (:head seen) 0 7) "; diffed against that"))
                        "\n")))
       (when (seq hidden)
         (str "\nHidden, unchanged since you saw them: "
              (str/join ", " (map #(str "`" (:path %) "`") hidden)) "\n"))))

(defn without-seen-section [notes]
  (str/trimr (first (str/split notes (re-pattern (str "(?m)^" seen-heading "$")) 2))))

(defn ticked-paths [notes]
  (map second (re-seq #"(?m)^- \[[xX]\] `([^`]+)`" notes)))

(defn record-seen [state branch ids head at]
  (reduce (fn [state [path blob]]
            (assoc-in state [branch path] {:blob blob :head head :at at}))
          state
          ids))

;; Review

(defn- fingerprint [f]
  (when (fs/exists? f)
    [(fs/size f) (str (fs/last-modified-time f))]))

(defn- wait-for-turn!
  "Blocks while another review window is open, so parallel calls queue instead of stacking windows."
  []
  (let [ch (java.nio.channels.FileChannel/open
            (fs/path (fs/temp-dir) "gram-review.lock")
            (into-array java.nio.file.OpenOption
                        [java.nio.file.StandardOpenOption/CREATE
                         java.nio.file.StandardOpenOption/WRITE]))]
    (when-not (.tryLock ch)
      (binding [*out* *err*] (println "Another review is open in Gram; waiting for it to close."))
      (.lock ch))))

(defn- parse-args [args]
  (loop [[a b & more :as args] args, opts {}]
    (cond
      (nil? a)            opts
      (= "--notes" a)     (recur more (assoc opts :notes b))
      (= "--pr" a)        (recur more (assoc opts :pr b))
      (= "--all" a)       (recur (rest args) (assoc opts :all true))
      (= "--dry-run" a)   (recur (rest args) (assoc opts :dry-run true))
      (#{"-h" "--help"} a) (do (println "usage: ,gram-review [--notes FILE] [--all] [--dry-run] [--pr N | BASE]")
                               (System/exit 0))
      :else               (recur (rest args) (assoc opts :base a)))))

(defn- diff-left
  "The read-only left side of a shown file's diff: the seen content if it has
  one still in the object store, else the base."
  [root tmp base {:keys [status path seen]}]
  (cond
    (and seen (= deleted (:blob seen)))
    (read-only-copy root (fs/path tmp "seen") nil path)

    (and seen (git root "cat-file" "-e" (:blob seen)))
    (read-only-copy root (fs/path tmp "seen") (:blob seen) path)

    :else
    (read-only-copy root (fs/path tmp "base") (when-not (= "A" status) (str base ":" path)) path)))

(defn- main [args]
  (let [{:keys [notes dry-run base pr all]} (parse-args args)
        _ (when (and pr base) (die "--pr picks its own base; drop" base))
        tmp (str (fs/create-temp-dir {:prefix "gram-review-"}))
        notes (if notes
                (str (fs/absolutize notes))
                (let [f (str (fs/path tmp "REVIEW.md"))]
                  (spit f "# Your notes\n\n")
                  f))
        [root base] (cond pr   (pr-checkout pr)
                          base [(toplevel) base]
                          :else (let [root (toplevel)] [root (default-base root)]))
        changed (sort-by second (changes root base))
        _ (when (empty? changed) (die "nothing differs from" base))
        branch (git root "rev-parse" "--abbrev-ref" "HEAD")
        state-file (seen-file root)
        ids (content-ids root (map second changed) false)
        {:keys [shown hidden]} (classify changed (get (load-seen state-file) branch) ids all)
        _ (when (empty? shown)
            (println "Nothing new to review: all" (count hidden)
                     "changed files are as the user marked them seen. Pass --all to show them anyway.")
            (System/exit 0))
        pairs (for [{:keys [status path] :as change} shown]
                [path
                 (diff-left root tmp base change)
                 (if (= "D" status)
                   (empty-file (fs/path tmp "deleted") path)
                   (str (fs/path root path)))])
        before (into {} (for [[path _ current] pairs] [path (fingerprint current)]))
        ;; Gram focuses the last tab it opens and opens diffs after plain paths,
        ;; so the notes go in with a second call that also waits for the window to close.
        cmds [(concat ["gram" "-n" root]
                      (mapcat (fn [[_ old new]] ["--diff" old new]) pairs))
              ["gram" "-a" "-w" root notes]]]
    (when-not (fs/exists? notes) (spit notes ""))
    (if dry-run
      (do (doseq [cmd cmds]
            (println (str/join " " (map #(if (re-find #"\s" %) (pr-str %) %) cmd))))
          (when (seq hidden)
            (println "hidden as seen:" (str/join " " (map :path hidden)))))
      (do
        (spit notes (str (without-seen-section (slurp notes)) "\n\n" (seen-section branch shown hidden)))
        (wait-for-turn!)
        (doseq [cmd cmds] (apply p/shell {:dir root} cmd))
        (let [text (slurp notes)
              edited (for [[path _ current] pairs
                           :when (not= (before path) (fingerprint current))]
                       path)
              ticked (filter (set (map :path shown)) (ticked-paths text))]
          (when (seq ticked)
            (spit state-file (pr-str (record-seen (load-seen state-file) branch
                                                  (content-ids root ticked true)
                                                  (git root "rev-parse" "HEAD")
                                                  (str (java.time.Instant/now))))))
          (println "Review window closed.\n")
          (println "## Worktree" root "\n")
          (println "## Notes" notes "\n")
          (println (without-seen-section text) "\n")
          (println "## Files the user edited\n")
          (println (if (seq edited) (str/join "\n" edited) "none") "\n")
          (println "## Files the user marked as seen\n")
          (println (if (seq ticked) (str/join "\n" ticked) "none")))))))

(when (= *file* (System/getProperty "babashka.file"))
  (main *command-line-args*))
