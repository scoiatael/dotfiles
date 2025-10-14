(ns server
  (:require [org.httpkit.server :as server]
            [cprop.core :refer [load-config]]
            [ring.middleware.proxy-headers :as proxy-headers]
            [ring.middleware.ssl :as ssl]
            [cprop.source :refer [from-env]]))

(defn handler [{:keys [remote-addr]}] {:body remote-addr})

(def config (load-config :file "config.edn" :merge [(from-env)]))

(def wrap-proxy
  (if (:proxy config) (fn [h] (-> h ssl/wrap-forwarded-scheme proxy-headers/wrap-forwarded-remote-addr)) (fn [h] h)))

(server/run-server (wrap-proxy handler) {:port (:port config)})

@(promise)
