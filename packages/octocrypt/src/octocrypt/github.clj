(ns octocrypt.github
  (:require [clojure.data.json :as json]
            [org.corfield.logging4j2 :as logger]
            [ring.util.codec :as codec :refer [url-encode]]
            [octocrypt.gpg :as gpg])
  (:import (java.net URI)
           (java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers)))

(defn- fetch-url [url]
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
    (let [response (fetch-url url)
          keys (json/read-str response :key-fn keyword)]
      (if (vector? keys)
        keys
        []))))


(defn fetch-gpg-keys [username]
  (let [url (str "https://api.github.com/users/" (url-encode username) "/gpg_keys")]
    (logger/with-log-context {:url url :username username}
      (logger/info "Requesting users GPG keys"))
    (let [response (fetch-url url)
          keys (json/read-str response :key-fn keyword)]
      (if (vector? keys)
        (mapv gpg/extract-key-details keys)
        []))))
