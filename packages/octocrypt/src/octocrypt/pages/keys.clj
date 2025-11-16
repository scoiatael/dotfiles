(ns octocrypt.pages.keys
    (:require
   [ring.util.codec :as codec :refer [url-encode]]
   [octocrypt.gpg :as gpg]
   [hiccup.util :refer [escape-html]]
   [hiccup.page :as page]))

(defn- render-ssh-keys [keys]
  (when (seq keys)
    [:div
     [:h2 "SSH Public Keys"]
     (for [{:keys [id created_at last_used title key]} keys]
       [:div.key-block
        [:div.title
         [:p [:strong "ID: "] id ]
         [:p [:a {:href (str "/encrypt/" (url-encode id))} "encrypt with this key"]]]
        (when title
          [:p [:strong "Title: "] (escape-html title)])
        [:p [:strong "Created at: "] created_at ]
        [:p [:strong "Last used: "] last_used ]
        [:pre.key-content key]])]))

(defn- render-gpg-keys [keys]
  (when (seq keys)
    [:div
     [:h2 "GPG Public Keys"]
     (for [{:keys [id raw_key expired? name email key_id fingerprint primary-uid key-type key-size created uids readable-output]} keys]
       [:div.key-block {:class (if expired? "expired" "")}
        [:div.title
         [:p [:strong "ID: "] id]
         [:p [:a {:href (str "/encrypt/" (url-encode id)) :class (if expired? "expired-link-disabled" "")} "encrypt with this key"]]]
        (when name
          [:p [:strong "Name: "] (escape-html name)])
        (when email
          [:p [:strong "Email: "] (escape-html email)])
        [:p [:strong "Key ID: "] key_id]
        ;; Display GPG CLI extracted details
        (when fingerprint
          [:p [:strong "Fingerprint: "] [:code fingerprint]])
        (when primary-uid
          [:p [:strong "Primary UID: "] (escape-html primary-uid)])
        (when key-type
          [:p [:strong "Key Type: "] (gpg/key-type->name key-type)
           (when key-size (str " (" key-size " bits)"))])
        (when created
          [:p [:strong "Created: "] (escape-html created)])
        (when (seq uids)
          [:div
           [:p [:strong "User IDs:"]]
           [:ul (for [uid uids]
                  [:li (escape-html uid)])]])
        (when  readable-output
          [:details
           [:summary "GPG Key Details"]
           [:pre.gpg-output (escape-html readable-output)]])
        [:pre.key-content (escape-html raw_key)]])]))

(defn page [username ssh-keys gpg-keys]
  (page/html5
   [:head
    [:title (str "Keys for " (escape-html username) " - GitHub Keys Fetcher")]
    (page/include-css "/style.css")]
   [:body
    [:div.back-link [:a {:href "/"} "Back to search"]]
    [:h1 (str "Public Keys for " (escape-html username))]
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
