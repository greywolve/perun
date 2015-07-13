(ns io.perun.markdown
  (:require [boot.util       :as u]
            [io.perun.core   :as perun]
            [me.raynes.conch.low-level :as sh]
            [clojure.java.io :as io]
            [clojure.string  :as str]
            [endophile.core  :as endophile]
            [clj-yaml.core   :as yaml]))

(defn substr-between
  "Find string that is nested in between two strings. Return first match.
  Copied from https://github.com/funcool/cuerdas"
  [s prefix suffix]
  (cond
    (nil? s) nil
    (nil? prefix) nil
    (nil? suffix) nil
    :else
    (some-> s
            (str/split prefix)
            second
            (str/split suffix)
            first)))

(defn parse-file-metadata [file-content]
  (if-let [metadata-str (substr-between file-content #"---\n" #"---\n")]
    (if-let [parsed-yaml (yaml/parse-string metadata-str)]
      ; we use `original` file flag to distinguish between generated files
      ; (e.x. created those by plugins)
      (assoc parsed-yaml :original true)
      {:original true})
    {:original true}))

(defn remove-metadata [content]
  (let [splitted (str/split content #"---\n")]
    (if (> (count splitted) 2)
      (first (drop 2 splitted))
      content)))

(defn markdown-to-html [file-content]
  (-> file-content
      remove-metadata
      endophile/mp
      endophile/to-clj
      endophile/html-string))

(defn markdown-to-html-external [cmd file-content]
  (let [p (apply sh/proc cmd)]
    (sh/feed-from-string p file-content)
    (sh/done p)
    (sh/stream-to-string p :out)))

(defn process-file-with [file f]
  (let [file-content (slurp file)]
    ; .getName returns only the filename so this should work cross platform
    (u/info "Processing Markdown: %s\n" (.getName file))
    [(.getName file) (merge (parse-file-metadata file-content)
                            {:content (f file-content)})]))

(defn parse-files-with [markdown-files f]
  (->> markdown-files
       (map #(-> % io/file (process-file-with f)))
       (into {})))

(defn parse-markdown [markdown-files cmd]
  (let [process-fn (if cmd
                     (partial markdown-to-html-external cmd)
                     markdown-to-html)
        parsed-files (parse-files-with markdown-files process-fn)]
    (u/info "Parsed %s markdown files\n" (count markdown-files))
    parsed-files))
