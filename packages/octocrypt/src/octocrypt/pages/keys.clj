(ns octocrypt.pages.keys
 (:require
  [ring.util.codec :as codec :refer [url-encode]]
  [hiccup.page :as page]))

(defn- render-ssh-keys [keys]
  (when (seq keys)
    [:div
     [:h2 "SSH Public Keys"]
     (for [key keys]
       (let [id (:id key)]
         [:div.key-block
          [:div.title
           [:p [:strong "ID: "] id ]
           [:p [:a {:href (str "/encrypt/" (url-encode id))} "encrypt with this key"]]]
          (when (:title key)
            [:p [:strong "Title: "] (:title key)])
          [:pre.key-content (:key key)]]))]))

(defn- render-gpg-keys [keys]
  (when (seq keys)
    [:div
     [:h2 "GPG Public Keys"]
     (for [key keys]
       (let [id (:id key)]
         [:div.key-block
          [:div.title
           [:p [:strong "ID: "] id ]
           [:p [:a {:href (str "/encrypt/" (url-encode id))} "encrypt with this key"]]]
          (when (:name key)
            [:p [:strong "Name: "] (:name key)])
          (when (:email key)
            [:p [:strong "Email: "] (:email key)])
          [:p [:strong "Key ID: "] (:key_id key)]
          [:pre.key-content (:raw_key key)]]))]))

(defn page [username ssh-keys gpg-keys]
    (page/html5
     [:head
      [:title (str "Keys for " username " - GitHub Keys Fetcher")]
      (page/include-css "/style.css")]
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
         (render-gpg-keys gpg-keys)])]))
