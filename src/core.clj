(ns core
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.java.io :as io]
            [clojure.data.csv :as csv]))

(defn remove-common-indent [s]
  (let [lines (str/split-lines s)
        non-blank-lines (remove str/blank? lines)
        indents (map #(count (re-find #"^\s*" %)) non-blank-lines)
        min-indent (apply min (cons Integer/MAX_VALUE indents))]
    (->> lines
         (map #(if (>= (count %) min-indent)
                 (subs % min-indent)
                 %))
         (str/join "\n"))))

(defn parse-record [record-text]
  (let [text (remove-common-indent record-text)
        lines (->> (clojure.string/split-lines text)
                   (remove clojure.string/blank?))]
    (loop [remaining-lines lines
           current-key nil
           acc {}]
      (if (empty? remaining-lines)
        acc
        (let [line (first remaining-lines)
              rest-lines (rest remaining-lines)]
          (if (or (clojure.string/starts-with? line " ")
                  (clojure.string/starts-with? line ">")
                  (clojure.string/starts-with? line ";"))
            ;; continuation line: append to current key's value, keeping leading char
            (if current-key
              (let [old-val (get acc current-key "")
                    continuation-text (str (subs line 0 1) (clojure.string/trim (subs line 1)))
                    new-val (str old-val "\n" continuation-text)
                    new-acc (assoc acc current-key new-val)]
                (recur rest-lines current-key new-acc))
              ;; no current key yet, skip line
              (recur rest-lines current-key acc))
            ;; new key line: parse key and initial value
            (let [[_ quoted-key rest] (re-matches #"^'([^']+)'\s*(.*)$" line)
                  [key val] (if quoted-key
                              [quoted-key rest]
                              (let [[k & v] (clojure.string/split line #"\s+" 2)]
                                [k (first v)]))
                  new-acc (assoc acc key (or val ""))]
              (recur rest-lines key new-acc))))))))



(defn remove-bom [s]
  (if (and (seq s) (= \uFEFF (first s)))
    (subs s 1)
    s))

(defn split-file-into-records [filepath]
  (let [content (-> filepath slurp remove-bom)
        records (->> (clojure.string/split content #"\n\s*\$\s*\n")
                     (map clojure.string/trim)
                     (remove clojure.string/blank?))]
    records))


(defn all-keys [records]
  (->> records
       (mapcat keys)
       set
       sort))

(defn all-have-same-keys? [records]
  (let [key-sets (map #(set (keys %)) records)
        unique-key-sets (set key-sets)]
    (= 1 (count unique-key-sets))))

(defn all-field-names [records]
  (->> records
       (mapcat keys)
       set
       (into (sorted-set))))

(defn parse-file-into-records [expected-fields filepath]
  (let [expected-set (set expected-fields)
        chunks (split-file-into-records filepath)
        records (map parse-record chunks)
        indexed-records (map-indexed vector records)
        ;; Find records with unexpected fields
        records-with-unexpected
        (->> indexed-records
             (filter (fn [[idx record]]
                       (let [fields (set (keys record))
                             unexpected (set/difference fields expected-set)]
                         (seq unexpected))))
             (map (fn [[idx record]]
                    (let [fields (set (keys record))
                          unexpected (set/difference fields expected-set)]
                      {:index idx
                       :unexpected-fields unexpected
                       :record record}))))]
    (if (seq records-with-unexpected)
      (throw (ex-info "Unexpected fields found in some records"
                      {:filepath filepath
                       :errors records-with-unexpected}))
      ;; else return records and unique fields
      {:records records
       :unique-fields (all-field-names records)})))


(def expected-fields
  ["Acquisition Date",
  "Acquisition Type",
  "Brief Description",
  "Classification",
  "Collection Type",
  "Condition",
  "Condition Date",
  "Condition Details",
  "Country Made",
  "Current Location",
  "Date Catalogued",
  "Date Entered into DB",
  "Date Made",
  "Date Modified",
  "Date Used",
  "Donor Name",
  "File",
  "History of Object",
  "Image",
  "Maker Name",
  "Materials",
  "Museum Code",
  "Name of Cataloguer",
  "Object Name",
  "Production Method",
  "Registration Number",
  "Size",
  "Subjects",
  "Title"])



(defn write-csv [filepath records]
  (let [headers (all-keys records)]
    (with-open [writer (io/writer filepath)]
      (csv/write-csv writer [(vec headers)])
      (doseq [record records]
        (csv/write-csv writer
                       [(map #(get record % "") headers)])))))

;; Usage example:
;;
;;
(comment
  (def text-chunks (split-file-into-records "C:\\Temp\\Iluka History Group\\test.dmp"))
  (println (first text-chunks))
  (def parsed-records (map parse-record text-chunks))
  (print (count parsed-records))
  (println parsed-records)
  (def first-record (first parsed-records))
  (print first-record)
  (type first-record)
  (keys first-record)
  (println text-chunks)
  ;;(write-csv "C:\\Temp\\Iluka History Group\\museum.csv" records)
  (def test-chunk
    "
    'Registration Number' 0010
    'Museum Code' IHG15.OO10B
    'Collection Type' MARITIME , FISHING
    'Object Name' cane fishing rod
    Subjects fishing rod
    'Brief Description' Rod belonged to Alex Cleland's father (also same name).
      Estimated 80-90 years old.  Used by both father and son to catch Blackfish ,
      Alex Jnr. last used it  in 1976 on Palmers Island, at the Fishing Haven
      Caravan Park.  NB. Alex's grandparents lived at Grafton. He spent his
      childhood on Heifer Station past Copmanhurst.  Heifer Station belonged to the
      Page Family.
      ")
  (def non-indented-test-chunk (remove-common-indent test-chunk))
  (parse-record non-indented-test-chunk)
  (map parse-record text-chunks)
  (doseq [line (clojure.string/split-lines non-indented-test-chunk)]
    (println (pr-str line)))
  (parse-file-into-records expected-fields "C:\\Temp\\Iluka History Group\\test.dmp")

  nil)









