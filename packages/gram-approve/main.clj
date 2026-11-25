;; Approval for tracker writes, given in Gram rather than in a permission prompt.
;;
;;   gram-approve PREVIEW.md   open the preview in Gram; record the ticked items
;;   gram-approve --hook       Claude Code PreToolUse hook for Bash and Linear saves
;;
;; A preview holds one section per item, headed `## [ ] <title>`. When the
;; window closes, the title and body of each ticked section are hashed into the
;; approvals file. The hook lets a Github or Linear write through only when every
;; text it sends (title, body) hashes to an approval from the last `approval-ttl`.
;; Writes that send no text, such as a status or label change, pass unchecked.
;; claude-bash-guard refuses writes to the approvals file, so only a tick in
;; Gram can add to it.

(require '[babashka.fs :as fs]
         '[babashka.process :as p]
         '[cheshire.core :as json]
         '[clojure.string :as str])

(def approval-ttl (java.time.Duration/ofHours 1))

(defn approvals-file []
  ;; Under ~/.claude because the nono sandbox lets Claude's shell write there.
  (or (System/getenv "GRAM_APPROVALS")
      (str (System/getenv "HOME") "/.claude/gram-approvals")))

(defn text-hash [s]
  (let [digest (.digest (java.security.MessageDigest/getInstance "SHA-256")
                        (.getBytes (str/trim (str/replace s "\r\n" "\n")) "UTF-8"))]
    (apply str (map #(format "%02x" %) digest))))

;; Preview

(def heading #"^## \[([ xX])\] (.*)$")

(defn sections
  "The preview's items as {:ticked? :title :body}."
  [text]
  (->> (str/split-lines text)
       (reduce (fn [items line]
                 (if-let [[_ mark title] (re-matches heading line)]
                   (conj items {:ticked? (not= " " mark) :title title :lines []})
                   (if (seq items)
                     (update-in items [(dec (count items)) :lines] conj line)
                     items)))
               [])
       (mapv (fn [{:keys [lines] :as item}]
               (-> item (dissoc :lines) (assoc :body (str/trim (str/join "\n" lines))))))))

(defn untick [text]
  (str/replace text #"(?m)^## \[[xX]\] " "## [ ] "))

(defn item-texts [{:keys [title body]}]
  (remove str/blank? [title body]))

(defn live-approvals
  "The approvals file's [epoch-second hash] entries younger than approval-ttl."
  [now]
  (let [f (approvals-file)
        cutoff (- (.getEpochSecond now) (.getSeconds approval-ttl))]
    (if (fs/exists? f)
      (->> (str/split-lines (slurp f))
           (keep #(let [[t h] (str/split % #" ")] (when h [(parse-long t) h])))
           (filter #(> (first %) cutoff)))
      [])))

(defn record! [hashes now]
  (let [f (approvals-file)
        entries (concat (live-approvals now)
                        (map #(vector (.getEpochSecond now) %) hashes))]
    (fs/create-dirs (fs/parent f))
    (spit f (str/join (map (fn [[t h]] (str t " " h "\n")) entries)))))

(defn review!
  "Opens the preview in Gram, unticked, and records what is ticked on close."
  [path]
  (spit path (untick (slurp path)))
  (p/shell "gram" "-n" "-w" path)
  (let [items (sections (slurp path))
        {ticked true unticked false} (group-by :ticked? items)]
    (record! (map text-hash (mapcat item-texts ticked)) (java.time.Instant/now))
    (println "Approved:" (if (seq ticked) (str/join "; " (map :title ticked)) "none"))
    (when (seq unticked)
      (println "Not approved:" (str/join "; " (map :title unticked))))
    (println (str "\nThe preview as approved is " path
                  "; send each item's title and body exactly as it stands there."))))

;; Hook

(def gh-writes
  {"issue" #{"create" "edit" "comment"}
   "pr"    #{"create" "edit" "comment" "review"}})

(defn flag-values
  "Values of the flags in names, given as `-f v` or `-f=v`."
  [args names]
  (keep (fn [[a b]]
          (if-let [[_ flag v] (re-matches #"(-[^=]+)=(.*)" a)]
            (when (names flag) v)
            (when (names a) b)))
        (partition-all 2 1 args)))

(defn gh-texts
  "The texts a lone `gh issue|pr <write>` sends, or nil for any other command."
  [command cwd]
  (let [[cmd group sub & args] (try (p/tokenize command) (catch Exception _ nil))]
    (when (and cmd (= "gh" (last (str/split cmd #"/")))
               (contains? (get gh-writes group) sub))
      (concat (flag-values args #{"-t" "--title"})
              (for [path (flag-values args #{"-F" "--body-file"})
                    :let [f (fs/path (or cwd ".") path)]]
                (if (fs/readable? f) (slurp (str f)) (str "(cannot read " path ")")))))))

(defn linear-texts [tool-input]
  (keep #(get tool-input %) ["title" "name" "description" "content"]))

(defn hook-reason
  "Deny reason for a PreToolUse event, or nil to pass it."
  [event now]
  (let [{:strs [tool_name tool_input cwd]} event
        linear? (str/starts-with? (str tool_name) "mcp__claude_ai_Linear__save_")
        texts (remove str/blank?
                      (cond linear?               (linear-texts tool_input)
                            (= "Bash" tool_name)  (gh-texts (get tool_input "command" "") cwd)))
        approved (set (map second (live-approvals now)))
        missing (remove #(approved (text-hash %)) texts)]
    (cond
      (and linear? (get tool_input "patch"))
      "send the whole description, not a patch, so it can be matched against its approved preview"

      (seq missing)
      (str "not approved in Gram: \"" (subs (str/trim (first missing)) 0 (min 80 (count (str/trim (first missing)))))
           "\". Write the item to a preview (`## [ ] <title>` sections), run gram-approve"
           " on it, and send the title and body exactly as approved"))))

(defn deny [reason]
  (println (json/generate-string
            {:hookSpecificOutput
             {:hookEventName "PreToolUse"
              :permissionDecision "deny"
              :permissionDecisionReason reason}})))

(when (= *file* (System/getProperty "babashka.file"))
  (let [[a] *command-line-args*]
    (cond
      (= "--hook" a)  (some-> (try (json/parse-string (slurp *in*)) (catch Exception _ nil))
                              (hook-reason (java.time.Instant/now))
                              deny)
      a               (review! a)
      :else           (do (println "usage: gram-approve PREVIEW.md | gram-approve --hook")
                          (System/exit 1)))))
