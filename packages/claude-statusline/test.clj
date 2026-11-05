;; Run with: bb test.clj

(require '[babashka.fs :as fs]
         '[babashka.process :as p]
         '[cheshire.core :as json]
         '[clojure.string :as str]
         '[clojure.test :refer [deftest is run-tests testing]])

(load-file (str (fs/parent (System/getProperty "babashka.file")) "/main.clj"))

(def example-json (let [file (-> "babashka.file" System/getProperty fs/parent (str "/example.json"))
                        contents (->> file fs/read-all-lines (str/join "\n"))]
                    (json/parse-string contents)))

(def example-statusline (statusline example-json))

(deftest shows-basic-info
  (is (str/includes? example-statusline "model: Opus")))

(let [{:keys [fail error]} (run-tests)]
  (when (pos? (+ fail error))
    (System/exit 1)))
