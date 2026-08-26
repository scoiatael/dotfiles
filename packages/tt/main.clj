;; tt — track work across Linear, Github, and Notion from the terminal.
;; Extracted from org-babel blocks in todo.org; `--format org` keeps those
;; org documents working via plain sh blocks.
;;
;;   tt linear issues|projects [--format table|org|json]
;;   tt linear api <graphql query>
;;   tt gh prs|todo            [--format table|org|json|md]
;;   tt gh comments [<pr>]     [--format table|org|json|md] [--all]
;;   tt notion tasks           [--format table|org|json]
;;
;; Auth: `pass linear-personal-api-token`, `pass notion-api-key`, and the
;; `gh` CLI's own login.

(require '[babashka.cli :as cli]
         '[babashka.http-client :as http]
         '[babashka.process :as p]
         '[cheshire.core :as json]
         '[clojure.string :as str])

(def linear-views
  {:issues   "8cc55187-a5b1-4c9b-8bc3-bbf76f21e30e"
   :projects "7980dca2-f1d4-4a97-89a6-613d1f615129"})

(def notion-db "1e7eb3917984806eb02aeba335f91644")
(def notion-me "10dd872b-594c-8178-874c-000233037a12")

(def linear-state-order
  {"Triage" 0 "Idea" 0 "Backlog" 1 "Paused" 1 "In Progress" 2 "In Review" 3})

(def notion-state-order {"Not started" 0 "Done" 3})

(defn die [& msg]
  (binding [*out* *err*] (apply println msg))
  (System/exit 1))

;; --- rendering ---------------------------------------------------------

(defn render-table [rows]
  (if (empty? rows)
    ""
    (let [rows (mapv #(mapv str %) rows)
          widths (apply mapv (fn [& col] (max 1 (apply max (map count col)))) rows)]
      (str/join "\n"
                (map (fn [row]
                       (str/trimr (str/join "  " (map #(format (str "%-" %2 "s") %1 %2) row widths))))
                     rows)))))

(defn render-org [rows]
  (str/join "\n"
            (map (fn [row]
                   ;; a literal | would break the org table cell
                   (str "| " (str/join " | " (map #(str/replace (str %) "|" "¦") row)) " |"))
                 rows)))

(defn- indent [s]
  (str/join "\n" (map #(str "  " %) (str/split-lines (str s)))))

(defn- comment-head [[author kind where created _ _]]
  (str/join "  " (remove str/blank? [author kind where (first (str/split (str created) #"T"))])))

(defn render-comments [rows]
  (str/join "\n\n" (map #(str (comment-head %) "\n" (indent (nth % 4))) rows)))

(defn render-comment-list [link]
  (fn [rows]
    (str/join "\n" (map #(str "- " (link (nth % 5) (comment-head %)) "\n" (indent (nth % 4)))
                        rows))))

(defn render-json [cols rows]
  (json/generate-string (map #(zipmap cols %) rows) {:pretty true}))

(defn emit [{:keys [cols rows render]} fmt]
  (let [custom (get render fmt)]
    (println
     (cond
       custom        (custom rows)
       (= fmt :json) (render-json cols rows)
       (= fmt :org)  (render-org rows)
       (= fmt :table) (render-table rows)
       :else (die "unknown format:" (name fmt))))))

;; --- linear ------------------------------------------------------------

(defn pass [entry]
  (try
    (str/trim (:out (p/shell {:out :string :err :string} "pass" entry)))
    (catch Exception _ (die "pass: could not read" entry))))

(defn linear-post [query]
  (let [resp (http/post "https://api.linear.app/graphql"
                        {:headers {"Authorization" (pass "linear-personal-api-token")
                                   "Content-Type" "application/json"}
                         :body (json/generate-string {:query query})
                         :throw false})
        body (try (json/parse-string (:body resp) true) (catch Exception _ nil))]
    (when (or (nil? body) (>= (:status resp) 400))
      (die "linear: HTTP" (:status resp) (:body resp)))
    body))

(def issues-query
  (format "query { customView(id: \"%s\") { issues { nodes {
             identifier state { name } title url } } } }"
          (:issues linear-views)))

(def projects-query
  (format "query { customView(id: \"%s\") { projects { nodes {
             id status { name } name url } } } }"
          (:projects linear-views)))

(defn issues-view [body]
  {:cols [:state :id :title :url]
   :rows (->> (get-in body [:data :customView :issues :nodes])
              (sort-by #(get linear-state-order (get-in % [:state :name]) -1))
              (map (juxt #(get-in % [:state :name]) :identifier :title :url)))})

(defn projects-view [body]
  {:cols [:status :id :name :url]
   :rows (->> (get-in body [:data :customView :projects :nodes])
              (sort-by #(get linear-state-order (get-in % [:status :name]) -1))
              (map (juxt #(get-in % [:status :name]) :id :name :url)))})

(defn linear-api! [query]
  (when (str/blank? query)
    (die "usage: tt linear api <graphql query>"))
  (let [body (linear-post query)]
    (println (json/generate-string body {:pretty true}))
    (when (:errors body) (System/exit 1))))

;; --- github ------------------------------------------------------------

(def pr-fields "number,repository,state,isDraft,title,author,createdAt,commentsCount,url")

(defn gh-json [& args]
  (try
    (json/parse-string (:out (apply p/shell {:out :string} "gh" args)) true)
    (catch Exception _ (die "gh failed:" (str/join " " args)))))

(defn gh-search [& args]
  (apply gh-json "search" "prs" "--state=open" "--json" pr-fields args))

(defn fetch-prs []
  {:requested (gh-search "--review-requested=@me")
   :mine (gh-search "--author=@me" "--owner" "WootingKB" "--archived=false")})

(defn prs-view [{:keys [requested mine]}]
  {:cols [:repo :state :type :title :author :created :comments :url]
   :rows (->> (concat requested mine)
              (sort-by #(if (:isDraft %) 0 1))
              (map (fn [pr]
                     [(get-in pr [:repository :name])
                      (:state pr)
                      (if (:isDraft pr) "draft" "review")
                      (:title pr)
                      (get-in pr [:author :login])
                      (:createdAt pr)
                      (str (:commentsCount pr) " comments")
                      (:url pr)])))})

(defn todo-view [mine]
  {:cols [:pr :title :url]
   :rows (->> mine
              (remove :isDraft)
              (map (fn [pr]
                     [(str (get-in pr [:repository :name]) "#" (:number pr))
                      (:title pr)
                      (:url pr)])))
   :render {:org (fn [rows]
                   (str/join "\n" (map (fn [[pr title url]]
                                         (str "- [[" url "][" pr "]] " title))
                                       rows)))
            :md (fn [rows]
                  (str/join "\n" (map (fn [[pr title url]]
                                        (str "- [" pr "](" url ") " title))
                                      rows)))}})

;; --- github comments ----------------------------------------------------

(def comments-query
  "query($owner:String!,$repo:String!,$number:Int!){
     repository(owner:$owner,name:$repo){ pullRequest(number:$number){
       comments(first:100){ pageInfo{ hasNextPage } nodes{ createdAt url body author{login __typename} } }
       reviews(first:100){ pageInfo{ hasNextPage } nodes{ createdAt url body state author{login __typename} } }
       reviewThreads(first:100){ pageInfo{ hasNextPage } nodes{ isResolved path line originalLine
         comments(first:50){ pageInfo{ hasNextPage }
           nodes{ createdAt url body author{login __typename} } } } } } } }")

;; integration comments posted under a human account, so __typename won't out them
(def bot-noise #"(?m)^<!-- \w+-pr-comment|^<!-- linear-|This stack of pull requests is managed by")

(defn- gh-pr-args [arg]
  ;; gh itself takes a number, url or branch; owner/repo#n needs splitting into -R
  (if-let [[_ repo number] (re-find #"^([^/\s]+/[^/#\s]+)#(\d+)$" (str arg))]
    [number "-R" repo]
    (when arg [(str arg)])))

(defn pr-ref [arg]
  ;; gh's own diagnosis of an unresolvable ref beats anything we could infer, so keep it
  (let [{:keys [exit out err]} (apply p/shell {:out :string :err :string :continue true}
                                      "gh" "pr" "view" (concat (gh-pr-args arg) ["--json" "url"]))
        url (when (zero? exit) (:url (json/parse-string out true)))
        [_ owner repo number] (re-find #"github\.com/([^/]+)/([^/]+)/pull/(\d+)" (str url))]
    (when-not number
      (die (str/join "\n"
                     (remove str/blank?
                             [(if arg
                                (str "no PR matching " arg)
                                (str "no PR for the current branch of "
                                     (System/getProperty "user.dir")))
                              (str/trim (str err))
                              "give a PR as a number, url or owner/repo#n"]))))
    {:owner owner :repo repo :number (parse-long number)}))

(defn- truncated? [pr]
  (or (some #(get-in pr [% :pageInfo :hasNextPage]) [:comments :reviews :reviewThreads])
      (some #(get-in % [:comments :pageInfo :hasNextPage]) (get-in pr [:reviewThreads :nodes]))))

(defn fetch-comments [{:keys [owner repo number]}]
  (let [body (gh-json "api" "graphql"
                      "-F" (str "owner=" owner) "-F" (str "repo=" repo) "-F" (str "number=" number)
                      "-f" (str "query=" comments-query))]
    (when (truncated? (get-in body [:data :repository :pullRequest]))
      (binding [*out* *err*]
        (println "warning: showing one page only (100 comments, 50 replies per thread);"
                 "this PR has more")))
    body))

(defn- comment-row [kind where c]
  [(get-in c [:author :login] "ghost") kind where (:createdAt c)
   (str/trim (str (:body c))) (:url c)])

(defn- worth-showing? [all? c]
  (or all?
      (and (not= "Bot" (get-in c [:author :__typename]))
           (not (re-find bot-noise (str (:body c)))))))

(defn- review-entry [keep? r]
  ;; a bodyless COMMENTED review is just the envelope around its inline comments
  (when (and (keep? r) (not (and (str/blank? (str (:body r)))
                                 (= "COMMENTED" (:state r)))))
    [(comment-row (str/lower-case (str (:state r))) nil r)]))

(defn- thread-entry [keep? t]
  (let [where (str (:path t) ":" (or (:line t) (:originalLine t)))
        kind (if (:isResolved t) "resolved" "inline")]
    (->> (get-in t [:comments :nodes])
         (filter keep?)
         (map-indexed #(comment-row (if (zero? %1) kind "reply") where %2))
         seq)))

(defn comments-view [body {:keys [all]}]
  (let [pr (get-in body [:data :repository :pullRequest])
        keep? (partial worth-showing? all)
        threads (cond->> (get-in pr [:reviewThreads :nodes])
                  (not all) (remove :isResolved))]
    {:cols [:author :kind :where :created :body :url]
     ;; entries stay contiguous so a thread's replies follow its first comment
     :rows (->> (concat (keep #(review-entry keep? %) (get-in pr [:reviews :nodes]))
                        (keep #(thread-entry keep? %) threads)
                        (map #(vector (comment-row "comment" nil %))
                             (filter keep? (get-in pr [:comments :nodes]))))
                (sort-by #(nth (first %) 3))
                (apply concat))
     :render {:table render-comments
              :org (render-comment-list #(str "[[" %1 "][" %2 "]]"))
              :md (render-comment-list #(str "[" %2 "](" %1 ")"))}}))

;; --- notion ------------------------------------------------------------

(defn notion-post []
  (let [resp (http/post (str "https://api.notion.com/v1/databases/" notion-db "/query")
                        {:headers {"Authorization" (str "Bearer " (pass "notion-api-key"))
                                   "Content-Type" "application/json"
                                   "Notion-Version" "2022-06-28"}
                         :body (json/generate-string
                                {:filter {:and [{:property "Responsible"
                                                 :people {:contains notion-me}}
                                                {:property "Status"
                                                 :status {:does_not_equal "Done"}}]}})
                         :throw false})]
    (when (>= (:status resp) 400)
      (die "notion: HTTP" (:status resp) (:body resp)))
    (json/parse-string (:body resp) true)))

(defn notion-view [body]
  {:cols [:status :name :url]
   :rows (->> (:results body)
              (sort-by #(get notion-state-order
                             (get-in % [:properties :Status :status :name]) 1))
              (map (juxt #(get-in % [:properties :Status :status :name])
                         #(get-in % [:properties :Name :title 0 :text :content])
                         :url)))})

;; --- cli ---------------------------------------------------------------

(def format-spec
  {:format {:coerce :keyword :alias :f :default :table}})

(defn usage! [_]
  (binding [*out* *err*]
    (println (str/trim "
usage: tt <command> [--format table|org|json]

  tt linear issues            my open issues (custom view)
  tt linear projects          my projects (custom view)
  tt linear api <query>       raw GraphQL query, prints JSON
  tt gh prs                   open PRs: review-requested + authored
  tt gh todo                  authored non-draft PRs (--format md|org lists)
  tt gh comments [<pr>]       review comments on a PR (default: current branch);
                              <pr> is a number, url or owner/repo#n. --all keeps
                              resolved threads and bot chatter
  tt notion tasks             my unfinished Notion tasks")))
  (System/exit 2))

(defn dispatch-table []
  (let [fmt #(get-in % [:opts :format])]
    [{:cmds ["linear" "issues"]   :spec format-spec
      :fn #(emit (issues-view (linear-post issues-query)) (fmt %))}
     {:cmds ["linear" "projects"] :spec format-spec
      :fn #(emit (projects-view (linear-post projects-query)) (fmt %))}
     {:cmds ["linear" "api"]
      :fn #(linear-api! (str/join " " (:args %)))}
     {:cmds ["gh" "prs"]          :spec format-spec
      :fn #(emit (prs-view (fetch-prs)) (fmt %))}
     {:cmds ["gh" "todo"]         :spec format-spec
      :fn #(emit (todo-view (:mine (fetch-prs))) (fmt %))}
     ;; :args->opts, or babashka.cli would treat flags after the pr ref as trailing args
     {:cmds ["gh" "comments"]     :spec (assoc format-spec :all {:coerce :boolean})
      :args->opts [:pr]
      :fn #(emit (comments-view (fetch-comments (pr-ref (get-in % [:opts :pr]))) (:opts %))
                 (fmt %))}
     {:cmds ["notion" "tasks"]    :spec format-spec
      :fn #(emit (notion-view (notion-post)) (fmt %))}
     {:cmds [] :fn usage!}]))

(when (= *file* (System/getProperty "babashka.file"))
  (cli/dispatch (dispatch-table) *command-line-args*))
