(ns core
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.data.csv :as csv]))

(defn parse-record [record-text]
  (let [lines (->> (str/split-lines record-text)
                   (map str/trim)
                   (remove str/blank?))]
    (reduce (fn [m line]
              ;; Match lines like: 'Key' value or Key value
              ;; Keys may be quoted with single quotes, values may contain spaces.
              (let [[_ raw-key raw-val] (re-matches #"^'([^']+)'\s*(.*)$" line)
                    [k v] (if raw-key
                            [raw-key raw-val]
                            ;; If no quotes, split first word as key, rest as value
                            (let [parts (str/split line #"\s+" 2)]
                              [(first parts) (second parts)]))]
                (if (and k v)
                  (assoc m (str/trim k) (str/trim v))
                  m)))
            {}
            lines)))

(defn parse-file [filepath]
  (let [content (slurp filepath)
        ;; Split on lines that contain only '$' (optional whitespace)
        records (->> (str/split content #"\n\s*\$\s*\n")
                     (map str/trim)
                     (remove str/blank?))]
    (map parse-record records)))

(defn all-keys [records]
  (->> records
       (mapcat keys)
       set
       sort))

(defn write-csv [filepath records]
  (let [headers (all-keys records)]
    (with-open [writer (io/writer filepath)]
      (csv/write-csv writer [(vec headers)])
      (doseq [record records]
        (csv/write-csv writer
                       [(map #(get record % "") headers)])))))

;; Usage example:
;; (def records (parse-file "data.txt"))
;; (write-csv "output.csv" records)

