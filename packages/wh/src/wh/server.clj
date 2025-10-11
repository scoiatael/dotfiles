(ns wh.server 
  (:require [org.httpkit.server :as srv]
            [clojure.math :as math]
            [wh.age :as age]
            [ring.middleware.file :as file]
            [clojure.pprint :as pprint]
            [cprop.core :refer [load-config]]
            [cprop.source :refer [from-env]]
            [ring.middleware.params :as params]
            [ring.middleware.cookies :as cookies]
            [ring.middleware.proxy-headers :as proxy-headers]
            [ring.middleware.ssl :as ssl]
            [babashka.json :as json]
            [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [pod.babashka.go-sqlite3 :as sqlite]
            [wh.uuid :as uuid]
            [honeysql.core :as sql]
            [honeysql.helpers :as helpers]
            [hiccup.page :as page]))

(def db (atom ":memory:"))

(defn- prepare-db []
  (sqlite/execute! @db ["create table if not exists channels (id TEXT, public_key TEXT, created_at TEXT)"])
  (sqlite/execute! @db ["create table if not exists webhooks (id TEXT, channel_id TEXT, payload TEXT)"]))

(defn template [& body]
  (page/html5
   [:head
    [:title "Webhook2RSS"]
    (page/include-css "/style.css")]
   [:body
    [:section.todoapp
     [:header.header
      [:h1 "Webhook2RSS"]]
     body]]))

(defn app-index [_req]
  (template [:div.form-container
             [:form {:method "POST" :action "/channel"}
              [:input {:type "submit" :value "Generate new webhook channel"}]]]
            [:p "To use this app you first need a channel to receive webhooks."]))

(defn insert-channel [id {:keys [public-key created-at]}]
  (-> (helpers/insert-into :channels)
      (helpers/columns :id :public-key :created-at)
      (helpers/values
       [[id public-key created-at]])
      sql/format))

(defn insert-webhook [{:keys [channel-id id payload]}]
  (-> (helpers/insert-into :webhooks)
      (helpers/columns :id :channel-id :payload)
      (helpers/values
       [[id channel-id payload]])
      sql/format))

(defn get-webhooks [{:keys [channel-id offset limit]}]
  (sql/format {:select [:id :payload]
               :from   [:webhooks]
               :offset offset
               :limit limit
               :where  [:= :channel-id channel-id]}))

(defn self-channel-url [id {:keys [headers scheme]}]
  (let [host (-> headers (get "host"))]
    (str (name scheme) "://" host "/channel/" id)))

(defn self-webhook-url [id {:keys [headers scheme]}]
  (let [host (-> headers (get "host"))]
    (str (name scheme) "://" host "/webhook/" id)))

(defn post-channel [{:as req}]
  (let [id (uuid/gen-uuid)
        post-url (self-webhook-url id req)
        self-url (self-channel-url id req)
        key (age/keygen) ]
    (sqlite/execute! @db (insert-channel id key))
    {:cookies {id (:secret-key key)}
     :body (template [:div
                      [:h3 "Channel setup"]
                      [:div.key-block
                       [:p "POST your webhooks to: " [:code post-url]]]
                      [:div.key-block
                       [:p "Your webhook private key is: "]
                       [:pre.key-content (:secret-key key)]]
                      [:div.key-block
                       [:p "Read it on " [:a {:href self-url} [:code self-url]]]] ])}))

(defn- serialize [{:keys [remote-addr start-time headers content-length websocket? content-type character-encoding uri server-name query-string scheme request-method body]} public-key]
  (->> (json/write-str
        {:remote-addr remote-addr
         :start-time start-time
         :headers headers
         :content-length content-length
         :websocket? websocket?
         :content-type content-type
         :character-encoding character-encoding
         :uri uri
         :server-name server-name
         :query-string query-string
         :scheme scheme
         :request-method request-method
         :body (when body (slurp body))})
       (age/encrypt public-key)))

(defn post-webhook [channel-id req]
  (if-let [public-key (-> (sqlite/query @db (sql/format {:select [:public-key] :from [:channels] :where [:= :id channel-id]})) first :public_key)]
    (let [id (uuid/gen-uuid)
          insert-sql (insert-webhook {:channel-id channel-id :id id :payload (serialize req public-key)})   ]
      (sqlite/execute! @db insert-sql)
      {:status 204})
    {:status 400}))

(defn render-webhook [{:keys [id payload]} secret-key]
  (let [parsed (->> payload (age/decrypt secret-key) json/read-str)
        rest (dissoc parsed :body) 
        body (:body parsed)
        out (java.io.StringWriter.)]
    (pprint/pprint rest out)
    [:div.key-block
     [:div.title
      [:p [:strong "ID: "] id ]]
     [:div [:p "Request details & headers"]
      [:pre.key-content (.toString out)]]
     (when body [:div [:p "Request content"] [:pre.key-content body]])]))

(defn get-channel [id {:keys [params cookies]}]
  (if-let [channel-cookie (get cookies id)]
    (let [offset (Integer/parseInt (get params "offset" "0"))
          secret-key (:value channel-cookie)
          limit (min (Integer/parseInt (get params "limit" "20")) 100)
          total (-> (sqlite/query @db (sql/format {:select [[:%count.* :total]] :from   [:webhooks] :where  [:= :channel-id id]})) first :total)
          webhooks (sqlite/query @db (get-webhooks {:channel-id id :offset offset :limit limit}))
          next-page-url (str "?offset=" (+ limit offset) "&limit=" limit)
          prev-page-url (str "?offset=" (- offset limit) "&limit=" limit)
          pages (int (math/floor (/ total limit)))
          current-page (int (math/ceil  (/ offset limit)))               ]
      {:body
       (template [:div
                  [:h3 "Channel " id]
                  [:div
                   [:h4 "Recorded " total " webhooks"]]
                  [:div.summary
                   [:div.back-link [:a {:disabled (not (< 0 current-page)) :href prev-page-url} "Previous"]]
                   [:span "page " (inc current-page) " out of " (inc pages)]
                   [:div.back-link [:a {:disabled (not (> pages  current-page)) :href next-page-url} "Next"]]]
                  (for [webhook webhooks]
                    (render-webhook webhook secret-key))])})
    {:status 400 :body "Missing channel auth cookie"}))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routes
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn routes [{:keys [request-method uri] :as req}]
  (let [path (vec (rest (str/split uri #"/")))]
    (match [request-method path]
      [:get []] {:body (app-index req)}
      [:post ["channel"]] (post-channel req)
      [:post ["webhook" id]] (post-webhook id req)
      [:get ["channel" id]] (get-channel id req)
      :else {:status 404 :body "Error 404: Page not found"})))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Server
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn -main [& args]
  (let [config (load-config :merge [(from-env)])
        port (:port config)
        db-location (:db config)
        proxy (:proxy config)
        wrap-proxy (if proxy (fn [h] (-> h ssl/wrap-forwarded-scheme proxy-headers/wrap-forwarded-remote-addr)) (fn [h] h))
        db-conn (sqlite/get-connection db-location) ]
    (reset! db db-conn)
    (prepare-db)
    (srv/run-server (-> routes (file/wrap-file "./public") params/wrap-params wrap-proxy cookies/wrap-cookies) {:port port})
    @(promise)))
