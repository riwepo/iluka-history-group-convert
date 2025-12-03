(ns core
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.pprint :refer [pprint]]
            [clojure.java.io :as io]
            [clojure.data.csv :as csv]))

(defn remove-common-indent
  "Removes the common leading whitespace from all lines in the string `s`.
   Useful for handling indented multiline strings.
   Returns the unindented string."
  [s]
  (let [lines (str/split-lines s)
        non-blank-lines (remove str/blank? lines)
        indents (map #(count (re-find #"^\s*" %)) non-blank-lines)
        min-indent (apply min (cons Integer/MAX_VALUE indents))]
    (->> lines
         (map #(if (>= (count %) min-indent)
                 (subs % min-indent)
                 %))
         (str/join "\n"))))

(defn parse-record
  "Parses a chunk of text into a map of key-value pairs.
   - `record-text`: string containing a single record with lines.
   Handles multiline values where subsequent lines start with space, '>', or ';'.
   Preserves leading characters in continuation lines."
  [record-text]
  (let [text (remove-common-indent record-text)
        lines (->> (str/split-lines text)
                   (remove str/blank?))]
    (loop [remaining-lines lines
           current-key nil
           acc {}]
      (if (empty? remaining-lines)
        acc
        (let [line (first remaining-lines)
              rest-lines (rest remaining-lines)]
          (if (or (str/starts-with? line " ")
                  (str/starts-with? line ">")
                  (str/starts-with? line ";"))
            ;; continuation line: append to current key's value, keeping leading char
            (if current-key
              (let [old-val (get acc current-key "")
                    continuation-text (str (subs line 0 1) (str/trim (subs line 1)))
                    new-val (str old-val "\n" continuation-text)
                    new-acc (assoc acc current-key new-val)]
                (recur rest-lines current-key new-acc))
              ;; no current key yet, skip line
              (recur rest-lines current-key acc))
            ;; new key line: parse key and initial value
            (let [[_ quoted-key rest] (re-matches #"^'([^']+)'\s*(.*)$" line)
                  [key val] (if quoted-key
                              [quoted-key rest]
                              (let [[k & v] (str/split line #"\s+" 2)]
                                [k (first v)]))
                  new-acc (assoc acc key (or val ""))]
              (recur rest-lines key new-acc))))))))



(defn remove-bom
  "Removes Byte Order Mark (U+FEFF) from the start of string `s` if present."
  [s]
  (if (and (seq s) (= \uFEFF (first s)))
    (subs s 1)
    s))

(defn split-file-into-records
  "Reads a file at `filepath`, removes BOM if present, and splits content into chunks
   separated by lines containing only '$'. Returns a sequence of record text chunks."
  [filepath]
  (let [content (-> filepath slurp remove-bom)
        records (->> (str/split content #"\n\s*\$\s*\n")
                     (map str/trim)
                     (remove str/blank?))]
    records))


(defn all-keys
  "Returns a sorted sequence of all unique keys from a sequence of records."
  [records]
  (->> records
       (mapcat keys)
       set
       sort))

(defn all-field-names
  "Returns a sorted set of all unique field names (keys) across all records."
  [records]
  (->> records
       (mapcat keys)
       set
       (into (sorted-set))))

(defn parse-file-into-records
  "Parses a file at `filepath`, validates that all records contain only expected fields.
   - `expected-fields`: a sequence of field names to validate against.
   - Returns a map with:
     :records => list of parsed records (maps),
     :unique-fields => set of all field names present across records.
   Throws an exception if any record contains unexpected fields."
  [expected-fields filepath]
  (let [expected-set (set expected-fields)
        chunks (split-file-into-records filepath)
        records (map parse-record chunks)
        indexed-records (map-indexed vector records)
        ;; Find records with unexpected fields
        records-with-unexpected
        (->> indexed-records
             (filter (fn [[_ record]]
                       (let [fields (set (keys record))
                             unexpected (set/difference fields expected-set)]
                         (seq unexpected))))
             (map (fn [[idx record]]
                    (let [fields (set (keys record))
                          unexpected (set/difference fields expected-set)]
                      {:index             idx
                       :unexpected-fields unexpected
                       :record            record}))))]
    (if (seq records-with-unexpected)
      (throw (ex-info "Unexpected fields found in some records"
                      {:filepath filepath
                       :errors   records-with-unexpected}))
      ;; else return records and unique fields
      {:records       records
       :unique-fields (all-field-names records)})))


(def expected-fields
  ["Acquisition Comments",
   "Acquisition Date",
   "Acquisition Type",
   "Brief Description",
   "Classification",
   "Collection Type",
   "Condition",
   "Condition Date",
   "Condition Details",
   "Conservation Report",
   "Country Made",
   "Country Used",
   "Current Location",
   "Date Catalogued",
   "Date Donor Form Sign",
   "Date Entered into DB",
   "Date Made",
   "Date Modified",
   "Date Used",
   "Donor Name",
   "Edition",
   "File",
   "History of Object",
   "Image",
   "Inscriptions",
   "Journal Title",
   "Maker Details",
   "Maker Name",
   "Materials",
   "Museum Code",
   "Name of Cataloguer",
   "Name of Data Enterer",
   "Negative Number",
   "Object Name",
   "Other Information",
   "Other Number",
   "Pages",
   "Physical Description",
   "Place of Publication",
   "Printer",
   "Production Method",
   "Publisher",
   "Purchase Price",
   "References",
   "Restrictions",
   "Region-State Made",
   "Region-State Used",
   "Registration Number",
   "Series Name",
   "Series Number",
   "Size",
   "Storage Comments",
   "Storage Location",
   "Subjects",
   "Supplementary File",
   "Title"
   "Town-Other Made"
   "Town-Other Used"])



(defn write-csv
  "Writes a sequence of records (maps) to a CSV file at `filepath`.
   The headers are derived from all keys present in the records."
  [filepath records]
  (let [headers (all-keys records)]
    (with-open [writer (io/writer filepath)]
      (csv/write-csv writer [(vec headers)])
      (doseq [record records]
        (csv/write-csv writer
                       [(map #(get record % "") headers)])))))


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
  (def records (:records (parse-file-into-records expected-fields "C:\\Temp\\Iluka History Group\\museum.dmp")))
  (def test-records [{"fred" 1 "bill" 2} {"fred" 1 "bill" 2}])
  (def one-record (take 1 records))
  (count one-record)
  one-record
  (write-csv "C:\\Temp\\Iluka History Group\\museum.csv" records)
  (pprint records)
  nil)









