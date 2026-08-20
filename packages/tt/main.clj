;; tt — track work across Linear, Github, and Notion from the terminal.
;; Extracted from org-babel blocks in todo.org; `--format org` keeps those
;; org documents working via plain sh blocks.
;;
;;   tt linear issues|projects [--format table|org|json]
;;   tt linear api <graphql query>
;;   tt gh prs|todo            [--format table|org|json|md]
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

(defn gh-search [& args]
  (try
    (json/parse-string
     (:out (apply p/shell {:out :string} "gh" "search" "prs" "--state=open"
                  "--json" pr-fields args))
     true)
    (catch Exception _ (die "gh search failed:" (str/join " " args)))))

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
     {:cmds ["notion" "tasks"]    :spec format-spec
      :fn #(emit (notion-view (notion-post)) (fmt %))}
     {:cmds [] :fn usage!}]))

(when (= *file* (System/getProperty "babashka.file"))
  (cli/dispatch (dispatch-table) *command-line-args*))
