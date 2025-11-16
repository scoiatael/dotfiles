(ns octocrypt.pages.encrypt
  (:require
   [clojure.string :refer [upper-case]]
   [hiccup.page :as page]
   [hiccup.util :refer [escape-html]]
   [ring.util.codec :as codec :refer [url-encode]]))

(defn form-page [key-id {:keys [type raw_key username]}]
  (page/html5
   [:head
    [:title (str "Encrypt for " key-id " - GitHub Keys Fetcher")]
    (page/include-css "/style.css")]
   [:body
    [:div.back-link [:a {:href "/"} "Back to search"]]
    [:h1 (str "Encryption using key " (url-encode key-id) " ("  (-> type name upper-case) " of " (escape-html username) ")")]
    [:div.key-block
     [:p [:strong "ID: "] key-id]
     [:pre.key-content raw_key]]
    [:div.form-container
     [:form {:method "POST" :action (str "/encrypt/" (url-encode key-id))}
      [:label {:for "secret"} "Secret to encrypt: "]
      [:div
       [:textarea {:name "secret" :id "secret" :required true :placeholder "e.g., myprivatepassword" :style " min-width: 30em; min-height: 10em;"}]]
      [:input {:type "submit" :value "Encrypt"}]]]]))


(defn value-page [value key-id {:keys [type raw_key username]}]
  (page/html5
   [:head
    [:title (str "Encrypted for " key-id " - GitHub Keys Fetcher")]
    (page/include-css "/style.css")]
   [:body
    [:div.back-link [:a {:href "/"} "Back to search"]]
    [:h1 (str "Encryption using key " (url-encode key-id) " ("  (-> type name upper-case) " of " (url-encode username) ")")]
    [:div.key-block
     [:p [:strong "ID: "] key-id]
     [:pre.key-content raw_key]]
    [:div.encrypted (escape-html value)]]))
