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

(defn plain [s]
  (str/replace s #"\u001b\[[0-9;]*m" ""))

(defn render [status & {:keys [branch]}]
  (with-redefs [git-branch (constantly branch)]
    (plain (statusline status))))

(deftest renders-two-lines
  (is (= "[Opus] 📁 directory | 🌿 feature/auth\n██████████ 8% | $0.01 | ⏱️ 0m 45s"
         (render example-json :branch "feature/auth"))))

(deftest omits-branch-outside-git
  (is (not (str/includes? (render example-json) "🌿"))))

(deftest context-bar-fills-with-usage
  (is (str/starts-with? (context-bar 42) (str green "████" faint)))
  (is (str/includes? (context-bar 95) red)))

(deftest handles-missing-usage
  (is (str/includes? (render (dissoc example-json "context_window" "cost")) "0% | $0.00 | ⏱️ 0m 0s")))

(let [{:keys [fail error]} (run-tests)]
  (when (pos? (+ fail error))
    (System/exit 1)))
