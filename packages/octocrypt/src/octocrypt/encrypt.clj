(ns octocrypt.encrypt
  (:require [babashka.process :as bb]
            [org.corfield.logging4j2 :as logger]
            [clojure.core.match :refer [match]])
  (:import (java.io File)))

(def age-cmd "age")
(def gpg-cmd "gpg")

(defn- encrypt-with-ssh [ssh-key value]
  (let [process (bb/shell {:in value :out :string} age-cmd "--encrypt" "--armor" "--recipient" ssh-key)]
    [:ok (:out process)]))


(defn- encrypt-with-gpg [gpg-key value]
  (let [tempfile (File/createTempFile "gpg" ".pub")
        _ (.deleteOnExit tempfile)
        _ (spit tempfile gpg-key)
        process (bb/shell {:continue true :in value :out :string :err :string} gpg-cmd "--encrypt" "--armor" "--recipient-file" (.getPath tempfile))]
    (.delete tempfile)
    (if (= 0 (:exit process))
      [:ok (:out process)]
      (do
        (logger/error "GPG encryption failed" process)
        [:error (:err process)]))))

(defn with-key [{:keys [type raw_key]} value]
  ((match type
          :ssh encrypt-with-ssh
          :gpg encrypt-with-gpg)
   raw_key value))
