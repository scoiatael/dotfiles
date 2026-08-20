;; Run with: bb test.clj
;; MAIN overrides the path to main.clj; BIN (optional) enables end-to-end
;; tests against an installed binary. Both are set by default.nix's checkPhase.

(require '[babashka.fs :as fs]
         '[babashka.process :as p]
         '[cheshire.core :as json]
         '[clojure.string :as str]
         '[clojure.test :refer [deftest is run-tests testing]])

(load-file (or (System/getenv "MAIN")
               (str (fs/parent (System/getProperty "babashka.file")) "/main.clj")))

(defn hook [url] (json/generate-string {:tool_input {:url url}}))

(def wl ["localhost:3000" "127.0.0.1:3000"])

(deftest allows-whitelisted-origins
  (is (nil? (decide wl (hook "http://localhost:3000/foo?x=1"))))
  (is (nil? (decide wl (hook "https://localhost:3000"))))
  (is (nil? (decide wl (hook "http://127.0.0.1:3000/"))))
  (is (nil? (decide wl (hook "http://LOCALHOST:3000/")))))

(deftest denies-other-origins
  (is (str/includes? (decide wl (hook "https://evil.com/")) "not in allowlist"))
  (is (str/includes? (decide wl (hook "http://localhost:3001/")) "not in allowlist"))
  (testing "host tricks"
    (is (some? (decide wl (hook "http://localhost:3000.evil.com/"))))
    (is (some? (decide wl (hook "http://localhost:3000@evil.com/"))))))

(deftest nothing-to-check-allows
  (is (nil? (decide wl (json/generate-string {:tool_input {}}))))
  (is (nil? (decide wl (hook ""))))
  (is (nil? (decide wl "not json"))))

(deftest unparseable-url-denies
  (is (str/includes? (decide wl (hook "http://[::bad")) "could not parse"))
  (is (str/includes? (decide wl (hook "notaurl")) "could not parse")))

(deftest scheme-handling
  (testing "scheme-less entries match http and https only"
    (is (nil? (decide ["example.com:8080"] (hook "https://example.com:8080/"))))
    (is (some? (decide ["localhost:3000"] (hook "file://localhost:3000/etc")))))
  (testing "pinned scheme must match"
    (is (nil? (decide ["http://example.com"] (hook "http://example.com/"))))
    (is (some? (decide ["http://example.com"] (hook "https://example.com/"))))))

(deftest default-ports
  (is (nil? (decide ["example.com"] (hook "https://example.com/"))))
  (is (nil? (decide ["example.com"] (hook "http://example.com/"))))
  (is (nil? (decide ["https://example.com"] (hook "https://example.com:443/"))))
  (is (some? (decide ["example.com"] (hook "https://example.com:8443/")))))

(deftest end-to-end
  (when-let [bin (System/getenv "BIN")]
    (testing "allow exits 0 with no output"
      (let [r (p/shell {:in (hook "http://localhost:3000/") :out :string :continue true}
                       bin "localhost:3000")]
        (is (zero? (:exit r)))
        (is (str/blank? (:out r)))))
    (testing "deny exits 0 with a permissionDecision"
      (let [r (p/shell {:in (hook "https://evil.com/") :out :string :continue true}
                       bin "localhost:3000")
            out (json/parse-string (:out r) true)]
        (is (zero? (:exit r)))
        (is (= "deny" (get-in out [:hookSpecificOutput :permissionDecision])))))
    (testing "no arguments is a usage error"
      (let [r (p/shell {:in "" :out :string :err :string :continue true} bin)]
        (is (= 2 (:exit r)))
        (is (str/includes? (:err r) "usage:"))))))

(let [{:keys [fail error]} (run-tests)]
  (when (pos? (+ fail error))
    (System/exit 1)))
