(ns octocrypt.encrypt
  (:require [babashka.process :as bb]
            [clojure.core.match :refer [match]])
  (:import (java.io File)))

(def age-cmd "age")
(def gpg-cmd "gpg")

(defn- encrypt-with-ssh [ssh-key value]
  (let [process (bb/shell {:in value :out :string} age-cmd "--encrypt" "--armor" "--recipient" ssh-key)]
    (:out process)))


(defn- encrypt-with-gpg [gpg-key value]
  (let [tempfile (File/createTempFile "gpg" ".pub")
        _ (.deleteOnExit tempfile)
        _ (spit tempfile gpg-key)
        process (bb/shell {:in value :out :string} gpg-cmd "--encrypt" "--armor" "--recipient-file" (.getPath tempfile))]
    (.delete tempfile)
    (:out process)))

(defn with-key [{:keys [type raw_key]} value]
  ((match type
          :ssh encrypt-with-ssh
          :gpg encrypt-with-gpg)
   raw_key value))
