(ns octocrypt.pages.keys
 (:require [octocrypt.github :as github]
            [hiccup.page :as page]))

(defn- render-ssh-keys [keys]
  (when (seq keys)
    [:div
     [:h2 "SSH Public Keys"]
     (for [key keys]
       [:div.key-block
        [:p [:strong "ID: "] (:id key)]
        (when (:title key)
          [:p [:strong "Title: "] (:title key)])
        [:pre.key-content (:key key)]])]))

(defn- render-gpg-keys [keys]
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

(defn page [username]
  (let [ssh-keys (github/fetch-ssh-keys username)
        gpg-keys (github/fetch-gpg-keys username)]
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
