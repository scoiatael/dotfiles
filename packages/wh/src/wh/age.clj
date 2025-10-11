(ns wh.age
  (:require [babashka.process :as process]
            [clojure.string :as str])
  (:import (java.io File)))

;; (def keygen-cmd "/nix/store/jqwrv77x8mz79i7bw09vsy6iwx07ihma-age-1.2.1/bin/age-keygen")
(def keygen-cmd "age-keygen")

;; (def age-cmd "/nix/store/jqwrv77x8mz79i7bw09vsy6iwx07ihma-age-1.2.1/bin/age")
(def age-cmd "age")

(defn- parse-keygen [acc line]
  (condp (comp seq re-seq) line
    #"# created: (.+)" :>> #(assoc acc :created-at (-> % first last))
    #"# public key: (.+)" :>> #(assoc acc :public-key (-> % first last))
    #"AGE-SECRET-KEY.+" (assoc acc :secret-key line)))

(defn keygen [] (let [keygen-lines (->> (process/shell {:out :string} keygen-cmd) :out str/split-lines)]
                  (reduce parse-keygen {} keygen-lines)))

;; (keygen)


(defn encrypt [public-key value]
  (let [process (process/shell {:in value :out :string} age-cmd "--encrypt" "--armor" "--recipient" public-key)]
    (:out process)))

(defn decrypt [secret-key value]
  (let [tempfile (File/createTempFile "age-" ".txt")
        _ (.deleteOnExit tempfile)
        _ (spit tempfile secret-key)
        process (process/shell {:in value :out :string} age-cmd "--decrypt" "--identity" (.getPath tempfile))]
    (.delete tempfile)
    (:out process)))

;; (let [{:keys [public-key secret-key]} (keygen)
;;       encrypted (encrypt public-key "test")]
;;   (prn encrypted)
;;   (prn (decrypt secret-key encrypted)))
