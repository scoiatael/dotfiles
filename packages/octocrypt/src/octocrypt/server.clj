(ns octocrypt.server
  (:require [clojure.string :as str]
            [clojure.data.json :as json]
            [org.corfield.logging4j2 :as logger]
            [ring.adapter.jetty :as jetty]
            [ring.middleware.params :as params]
            [ring.util.codec :as codec :refer [url-encode]]
            [hiccup.page :as page])
  (:import (java.net URI)
           (java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers)))

(defn fetch-url [url]
  (let [client (HttpClient/newHttpClient)
        request (-> (HttpRequest/newBuilder)
                    (.uri (URI/create url))
                    (.header "User-Agent" "Clojure GitHub Keys Fetcher")
                    (.build))]
    (-> client
        (.send request (HttpResponse$BodyHandlers/ofString))
        (.body))))

(defn fetch-ssh-keys [username]
  (let [url (str "https://api.github.com/users/" (url-encode username) "/keys")]
      (logger/with-log-context {:url url :username username}
        (logger/info "Requesting users SSH keys"))
    (try
      (let [response (fetch-url url)
            keys (json/read-str response :key-fn keyword)]
        (if (vector? keys)
          keys
          []))
      (catch Exception e
        (println (str "Error fetching SSH keys: " (.getMessage e)))
        []))))

(defn fetch-gpg-keys [username]
  (let [url (str "https://api.github.com/users/" (url-encode username) "/gpg_keys")]
        (logger/with-log-context {:url url :username username}
          (logger/info "Requesting users GPG keys"))
    (try
      (let [response (fetch-url url)
            keys (json/read-str response :key-fn keyword)]
        (if (vector? keys)
          keys
          []))
      (catch Exception e
        (println (str "Error fetching GPG keys: " (.getMessage e)))
        []))))

(defn render-ssh-keys [keys]
  (when (seq keys)
    [:div
     [:h2 "SSH Public Keys"]
     (for [key keys]
       [:div.key-block
        [:p [:strong "ID: "] (:id key)]
        (when (:title key)
          [:p [:strong "Title: "] (:title key)])
        [:pre.key-content (:key key)]])]))

(defn render-gpg-keys [keys]
  (when (seq keys)
    [:div
     [:h2 "GPG Public Keys"]
     (for [key keys]
       [:div.key-block
        [:p [:strong "ID: "] (:id key)]
        (when (:name key)
          [:p [:strong "Name: "] (:name key)])
        (when (:email key)
          [:p [:strong "Email: "] (:email key)])
        [:p [:strong "Key ID: "] (:key_id key)]
        [:pre.key-content (:raw_key key)]])]))

(defn home-page []
  (page/html5
   [:head
    [:title "GitHub Keys Fetcher"]
    [:style "
       body { font-family: Arial, sans-serif; max-width: 800px; margin: 0 auto; padding: 20px; }
       .form-container { background: #f5f5f5; padding: 20px; border-radius: 8px; margin-bottom: 20px; }
       .key-block { background: #f9f9f9; padding: 15px; margin: 10px 0; border-left: 4px solid #007cba; }
       .key-content { background: #333; color: #fff; padding: 10px; border-radius: 4px; overflow-x: auto; word-wrap: break-word; }
       input[type=text] { padding: 8px; font-size: 16px; width: 200px; }
       input[type=submit] { padding: 8px 16px; font-size: 16px; background: #007cba; color: white; border: none; border-radius: 4px; cursor: pointer; }
       input[type=submit]:hover { background: #005a8b; }
       .error { color: red; padding: 10px; background: #ffe6e6; border-radius: 4px; }
     "]]
   [:body
    [:h1 "GitHub Public Keys Fetcher"]
    [:div.form-container
     [:form {:method "GET" :action "/keys"}
      [:label {:for "username"} "GitHub Username: "]
      [:input {:type "text" :name "username" :id "username" :required true :placeholder "e.g., torvalds"}]
      [:input {:type "submit" :value "Fetch Keys"}]]]
    [:p "Enter a GitHub username to view their public SSH and GPG keys."]]))

(defn keys-page [username]
  (let [ssh-keys (fetch-ssh-keys username)
        gpg-keys (fetch-gpg-keys username)]
    (page/html5
     [:head
      [:title (str "Keys for " username " - GitHub Keys Fetcher")]
      [:style "
         body { font-family: Arial, sans-serif; max-width: 800px; margin: 0 auto; padding: 20px; }
         .form-container { background: #f5f5f5; padding: 20px; border-radius: 8px; margin-bottom: 20px; }
         .key-block { background: #f9f9f9; padding: 15px; margin: 10px 0; border-left: 4px solid #007cba; }
         .key-content { background: #333; color: #fff; padding: 10px; border-radius: 4px; overflow-x: auto; word-wrap: break-word; white-space: pre-wrap; }
         input[type=text] { padding: 8px; font-size: 16px; width: 200px; }
         input[type=submit] { padding: 8px 16px; font-size: 16px; background: #007cba; color: white; border: none; border-radius: 4px; cursor: pointer; }
         input[type=submit]:hover { background: #005a8b; }
         .back-link { margin-bottom: 20px; }
         .back-link a { color: #007cba; text-decoration: none; }
         .back-link a:hover { text-decoration: underline; }
       "]]
     [:body
      [:div.back-link [:a {:href "/"} "Back to search"]]
      [:h1 (str "Public Keys for " username)]
      [:div.form-container
       [:form {:method "GET" :action "/keys"}
        [:label {:for "username"} "GitHub Username: "]
        [:input {:type "text" :name "username" :id "username" :required true :placeholder "e.g., torvalds" :value username}]
        [:input {:type "submit" :value "Fetch Keys"}]]]
      (if (and (empty? ssh-keys) (empty? gpg-keys))
        [:p "No public keys found for this user."]
        [:div
         (render-ssh-keys ssh-keys)
         (render-gpg-keys gpg-keys)])])))

(defn error-page [message]
  (page/html5
   [:head
    [:title "Error - GitHub Keys Fetcher"]
    [:style "
       body { font-family: Arial, sans-serif; max-width: 800px; margin: 0 auto; padding: 20px; }
       .error { color: red; padding: 10px; background: #ffe6e6; border-radius: 4px; }
       .back-link { margin-top: 20px; }
       .back-link a { color: #007cba; text-decoration: none; }
       .back-link a:hover { text-decoration: underline; }
     "]]
   [:body
    [:h1 "Error"]
    [:div.error message]
    [:div.back-link [:a {:href "/"} "Back to search"]]]))

(defn handler [request]
  (let [uri (:uri request)
        params (:params request)]
    (cond
      (= uri "/")
      {:status 200
       :headers {"Content-Type" "text/html"}
       :body (home-page)}

      (= uri "/keys")
      (let [username (get params "username" )]
        (if (str/blank? username)
          {:status 400
           :headers {"Content-Type" "text/html"}
           :body (error-page "Username is required")}
          {:status 200
           :headers {"Content-Type" "text/html"}
           :body (keys-page username)}))

      :else
      {:status 404
       :headers {"Content-Type" "text/html"}
       :body (error-page "Page not found")})))

(def app
  (params/wrap-params handler))

(defn main [& args]
  (let [port (if-let [portArg (first args)] (Integer/parseInt portArg) 3000)]
    (println (str "Starting server on http://localhost:" port))
    (jetty/run-jetty app {:port port :join? true})))
