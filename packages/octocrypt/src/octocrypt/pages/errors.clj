(ns octocrypt.pages.errors
 (:require [hiccup.page :as page]))

(defn page [message]
  (page/html5
   [:head
    [:title "Error - GitHub Keys Fetcher"]
    (page/include-css "/style.css")]
   [:body
    [:h1 "Error"]
    [:div.error message]
    [:div.back-link [:a {:href "/"} "Back to search"]]]))
