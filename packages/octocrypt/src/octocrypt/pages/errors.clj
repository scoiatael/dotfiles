(ns octocrypt.pages.errors
 (:require [hiccup.page :as page]))

(defn page [message]
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
