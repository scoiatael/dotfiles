(ns octocrypt.pages.home
 (:require 
    [hiccup.page :as page]))

(defn page []
  (page/html5
   [:head
    [:title "GitHub Keys Fetcher"]
    (page/include-css "/style.css")]
   [:body
    [:h1 "GitHub Public Keys Fetcher"]
    [:div.form-container
     [:form {:method "GET" :action "/keys"}
      [:label {:for "username"} "GitHub Username: "]
      [:input {:type "text" :name "username" :id "username" :required true :placeholder "e.g., torvalds"}]
      [:input {:type "submit" :value "Fetch Keys"}]]]
    [:p "Enter a GitHub username to view their public SSH and GPG keys."]]))
