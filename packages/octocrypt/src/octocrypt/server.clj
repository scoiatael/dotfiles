(ns octocrypt.server
  (:require [clojure.string :as str]
            [org.httpkit.server :as srv]
            [org.corfield.logging4j2 :as logger]
            [octocrypt.pages.home :as home]
            [octocrypt.pages.keys :as keys]
            [octocrypt.pages.errors :as errors]
            [ring.middleware.params :as params]
            [clojure.core.match :refer [match]]))

(defn- routes [{:keys [request-method uri] :as req}]
  (let [path (vec (rest (str/split uri #"/")))]
    (match [request-method path]
           [:get []] {:body (home/page)}
           [:get ["keys"]] (if-let [username (-> req :params (get "username"))]
                             {:body (keys/page username)}
                             {:status 400 :body (errors/page "Username is required")})
           :else {:status 404 :body (errors/page "Page not found")})))

(defn main [& args]
  (let [port 3000]
    (logger/info "Starting on port" port "from" args)
    (srv/run-server (params/wrap-params routes) {:port port})))
