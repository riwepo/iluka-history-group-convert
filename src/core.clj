(ns core
  (:require [clojure.string :as str]
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
        lines (->> (str/split-lines text)
                   (remove str/blank?))]
    (loop [remaining-lines lines
           current-key nil
           acc {}]
      (if (empty? remaining-lines)
        acc
        (let [line (first remaining-lines)
              rest-lines (rest remaining-lines)]
          (if (str/starts-with? line " ")
            ;; continuation line: append to current key's value
            (if current-key
              (let [old-val (get acc current-key "")
                    new-val (str old-val "\n" (str/trim line))
                    new-acc (assoc acc current-key new-val)]
                (recur rest-lines current-key new-acc))
              ;; no key yet, skip line
              (recur rest-lines current-key acc))
            ;; new key line: parse key and initial value
            (let [[_ quoted-key rest] (re-matches #"^'([^']+)'\s*(.*)$" line)
                  [key val] (if quoted-key
                              [quoted-key rest]
                              (let [[k & v] (str/split line #"\s+" 2)]
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

  nil)









