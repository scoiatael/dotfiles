(ns octocrypt.server
  (:require [clojure.string :as str]
            [org.httpkit.server :as srv]
            [org.corfield.logging4j2 :as logger]
            [octocrypt.pages.home :as home]
            [octocrypt.pages.keys :as keys]
            [octocrypt.pages.encrypt :as encrypt-page]
            [octocrypt.encrypt :as encrypt]
            [octocrypt.pages.errors :as errors]
            [octocrypt.github :as github]
            [ring.middleware.params :as params]
            [ring.middleware.file :as file]
            [ring.util.response :refer [update-header]]
            [clojure.core.match :refer [match]]))

(def cache-control "Cache-Control")
(def cache-control-public "max-age=180, public")
(def user-keys (atom {}))

(defn- keys-to-map [type username keys]
  (into {}
        (for [{:keys [id raw_key key]} keys]
          [(str id) {:type type :username username :raw_key (or raw_key key)}])))

(defn- keys-controller [req]
  (if-let [username (-> req :params (get "username"))]
   (let [ssh-keys (github/fetch-ssh-keys username)
         gpg-keys (github/fetch-gpg-keys username)]
     (swap! user-keys (fn [old] (merge old (keys-to-map :ssh username ssh-keys) (keys-to-map :gpg username gpg-keys))))
     (logger/info "cache has" (.size @user-keys) "entries")
     {:body (keys/page username ssh-keys gpg-keys) :headers {cache-control cache-control-public}})
   {:status 400 :body (errors/page "Username is required")}) )

(defn- encrypt-get-controller [key-id req]
  (if-let [key (get @user-keys key-id)]
    {:body (encrypt-page/form-page key-id key) :headers {cache-control cache-control-public}}
    {:status 400 :body (errors/page "Key not found")}))

(defn- encrypt-post-controller [key-id req]
  (if-let [key (get @user-keys key-id)]
    (if-let [value (-> req :form-params (get "secret"))]
      {:body (encrypt-page/value-page (encrypt/with-key key value) key-id key) :headers {cache-control cache-control-public}}
      {:status 400 :body (errors/page "Missing value to encrypt")})
    {:status 400 :body (errors/page "Key not found")}))

(defn- routes [{:keys [request-method uri] :as req}]
  (let [path (vec (rest (str/split uri #"/")))]
    (match [request-method path]
           [:get []] {:body (home/page) :headers {cache-control cache-control-public}}
           [:get ["keys"]] (keys-controller req)
           [:get ["encrypt" key-id]] (encrypt-get-controller key-id req)
           [:post ["encrypt" key-id]] (encrypt-post-controller key-id req)
           :else {:status 404 :body (errors/page "Page not found")})))

(defn- with-public-server [handler]
  (fn [request]
    (if-let [response (file/file-request request "./public")]
      (update-header response cache-control (constantly cache-control-public))
      (handler request))))

(defn main [& args]
  (let [port 3000]
    (logger/info "Starting on port" port "from" args)
    (srv/run-server (-> routes params/wrap-params with-public-server)
                    {:port port
                     :error-logger (fn [msg ex] (logger/error msg ex))
                     :warn-logger (fn [msg ex] (logger/warn msg ex))
                     :event-logger (fn [ev-name] (logger/info ev-name))})))
