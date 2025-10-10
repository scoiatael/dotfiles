;; https://github.com/nalgeon/uuidv7/blob/main/src/uuidv7.clj
(ns wh.uuid
  (:import (java.security SecureRandom)))

(defn gen-uuid-v7 []
  (let [rand-array (byte-array 10)]
    (.nextBytes (SecureRandom.) rand-array)
    (concat
      ;; timestamp
      (map byte (.toByteArray (biginteger (System/currentTimeMillis))))
      ;; version
      [(bit-or (bit-and (first rand-array) 0x0F) 0x70)]
      [(nth rand-array 1)]
      ;; variant
      [(bit-or (bit-and (nth rand-array 2) 0x3F) 0x80)]
      (drop 3 rand-array))))

(defn uuid-to-string [uuid-bytes]
  (apply str (map #(format "%02x" %) uuid-bytes)))

(defn gen-uuid [] (uuid-to-string (gen-uuid-v7)))
