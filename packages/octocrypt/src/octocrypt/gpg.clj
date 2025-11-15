(ns octocrypt.gpg
  (:require [babashka.process :as process]
            [clojure.string :as str]
            [tick.core :as t]
            [org.corfield.logging4j2 :as logger]))

(def ^:private example-gpg-output "
pub:u:4096:1:1F748B25B736F0A8:1588338088:2092914088::u:::scESC::::::23::0:
fpr:::::::::EAB800957676ADBE2E29E1B61F748B25B736F0A8:
uid:u::::1588338088::FD4C87C245551CDFCF330270CAC5209AA1857C17::Lukasz Czaplinski <czaplinski.lukasz@pm.me>::::::::::0:
uid:u::::1588338088::1411C3A219AD48F9EF07675BE0848118B2E3258A::Lukasz Czaplinski <git@scoiatael.dev>::::::::::0:
uid:u::::1588338088::FE54E9A3C5BE130EF14D6D45FBB62BF992653E61::Lukasz Czaplinski <lukasz@czaplin.ski>::::::::::0:
sub:u:4096:1:A479B7097786412E:1588338088:2092914088:::::e::::::23:
fpr:::::::::954E21D29336CE157AE08336A479B7097786412E:")

(defn- parse-gpg-output
  "Parse gpg --show-keys --with-colons output to extract key details"
  [output]
  (let [lines (str/split-lines output)
        result (atom {:fingerprints []
                      :uids []
                      :subkeys []
                      :key-type nil
                      :key-size nil
                      :created nil
                      :expires nil})]
    (doseq [line lines]
      (let [trimmed (str/trim line)]
        (when-not (empty? trimmed)
          (let [fields (str/split trimmed #":")]
            (case (first fields)
              "pub" (swap! result assoc 
                           :key-type (nth fields 3 nil)
                           :key-size (nth fields 2 nil)
                           :created (nth fields 5 nil)
                           :expires (nth fields 6 nil))
              "fpr" (swap! result update :fingerprints conj (nth fields 9 nil))
              "uid" (swap! result update :uids conj (nth fields 9 nil))
              "sub" (swap! result update :subkeys conj {:key-id (nth fields 4 nil)
                                                        :key-size (nth fields 2 nil)
                                                        :created (nth fields 5 nil)
                                                        :expires (nth fields 6 nil)}))))))
    @result))

;; (parse-gpg-output example-gpg-output)

(defn- extract-primary-uid
  "Extract the primary UID (User ID) from GPG key details"
  [{:keys [uids]}]
  (first uids ))

(defn- extract-fingerprint
  "Extract the primary fingerprint from GPG key details"
  [{:keys [fingerprints]}]
  (first fingerprints))

(defn- subkey-expired? [{:keys [expires]}]  (t/< (-> expires Integer/parseInt (* 1000) t/instant ) (t/now)))

(subkey-expired? {:expires "2092914088"})

(defn- expired?
  [{:keys [subkeys]}]
  (some subkey-expired? subkeys))

(defn- post-process
  "Enrich a GPG key with details extracted from gpg CLI"
      [details]
  (assoc details
         :expired? (expired? details)
         :fingerprint (extract-fingerprint details)
         :primary-uid (extract-primary-uid details)))


(defn extract-key-details
  "Extract detailed information from a GPG public key using gpg --show-keys"
  [{:as key :keys [raw_key]}]
  (let [result-colons (:out (process/shell ["gpg" "--show-keys" "--with-colons" "--with-fingerprint"]
                                           {:in raw_key
                                            :out :string
                                            :err :string}))
        result-readable (:out (process/shell ["gpg" "--show-keys"]
                                             {:in raw_key
                                              :out :string
                                              :err :string}))]
    (merge
     key
     {:raw-output result-colons
      :readable-output result-readable}
     (-> result-colons parse-gpg-output post-process))))

(def ^:private gpg-key-type-map
  {"1"   "RSA"
   "2"   "RSA (encrypt only)"
   "3"   "RSA (sign only)"
   "16"  "ElGamal (encrypt only)"
   "17"  "DSA"
   "18"  "ECDH"
   "19"  "ECDSA"
   "20"  "ElGamal (encrypt and sign)"
   "21"  "DH (X9.42)"
   "22"  "Ed25519"
   "23"  "AEAD"})

(defn key-type->name
  "Convert GPG key type number to human-readable name.
   Returns 'Unknown' for unrecognized key types."
  [key-type]
  (get gpg-key-type-map key-type key-type))
