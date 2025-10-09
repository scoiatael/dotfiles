(ns octocrypt.pages.home
 (:require 
    [hiccup.page :as page]))

(defn page []
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
       .error { color: red; padding: 10px; background: #ffe6e6; border-radius: 4px; }"]]
   [:body
    [:h1 "GitHub Public Keys Fetcher"]
    [:div.form-container
     [:form {:method "GET" :action "/keys"}
      [:label {:for "username"} "GitHub Username: "]
      [:input {:type "text" :name "username" :id "username" :required true :placeholder "e.g., torvalds"}]
      [:input {:type "submit" :value "Fetch Keys"}]]]
    [:p "Enter a GitHub username to view their public SSH and GPG keys."]]))
