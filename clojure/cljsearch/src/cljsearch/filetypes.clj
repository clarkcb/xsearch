;;; ############################################################################
;;;
;;; filetypes.clj
;;;
;;; Utility functions for getting file extension and determining file type
;;;
;;; ############################################################################

(ns cljsearch.filetypes
  #^{:author "Cary Clark",
     :doc "Module to provide file-related utility functions"}
  (:import (java.io File))
  (:require [clojure.java.io :as io])
  (:require [clojure.data.json :as json])
  (:use [clojure.set :only (union)]
        [clojure.string :only (split lower-case)]
        [clojure.xml :only (parse)]
        [cljsearch.fileutil :only (expand-path get-ext)]))

(def ARCHIVE "archive")
(def BINARY "binary")
(def CODE "code")
(def TEXT "text")
(def XML "xml")

(defn get-filetypemap-from-xml []
  (let [ftcontents (slurp (io/resource "filetypes.xml"))
        ftstream (java.io.ByteArrayInputStream. (.getBytes ftcontents))
        filetypes (filter #(= :filetype (:tag %)) (xml-seq (parse ftstream)))
        typenames (map :name (map :attrs filetypes))
        extension-nodes (map first (map :content (map first (map :content filetypes))))
        extension-sets (map #(set %) (map #(split % #"\s+") extension-nodes))
        filetypemap (zipmap typenames extension-sets)
        textmap (hash-map "all-text"
                  (union (get filetypemap TEXT)
                         (get filetypemap CODE)
                         (get filetypemap XML)))
        searchablemap (hash-map "searchable"
                        (union (get filetypemap ARCHIVE)
                               (get filetypemap BINARY)
                               (get filetypemap TEXT)))
        ]
  (merge filetypemap textmap searchablemap)))

(defn get-filetypemap-from-json []
  (let [contents (slurp (io/resource "filetypes.json"))
        filetypes-objs (:filetypes (json/read-str contents :key-fn keyword))
        typenames (map :type filetypes-objs)
        extension-sets (map #(set %) (map :extensions filetypes-objs))
        filetypemap (zipmap typenames extension-sets)
        textmap (hash-map "all-text"
                  (union (get filetypemap TEXT)
                         (get filetypemap CODE)
                         (get filetypemap XML)))
        searchablemap (hash-map "searchable"
                        (union (get filetypemap ARCHIVE)
                               (get filetypemap BINARY)
                               (get filetypemap TEXT)))
        fullmap (merge filetypemap textmap searchablemap)
        ]
    fullmap))

(def FILETYPEMAP (get-filetypemap-from-json))

(defn archive-ext? [ext]
  (contains? (get FILETYPEMAP ARCHIVE) ext))

(defn archive-file? [f]
  (archive-ext? (get-ext f)))

(defn binary-ext? [ext]
  (contains? (get FILETYPEMAP BINARY) ext))

(defn binary-file? [f]
  (contains? (get FILETYPEMAP BINARY) (get-ext f)))

(defn code-ext? [ext]
  (contains? (get FILETYPEMAP CODE) ext))

(defn code-file? [f]
  (contains? (get FILETYPEMAP CODE) (get-ext f)))

(defn searchable-file? [f]
  (contains? (get FILETYPEMAP "searchable") (get-ext f)))

(defn text-ext? [ext]
  (contains? (get FILETYPEMAP TEXT) ext))

(defn text-file? [f]
  (contains? (get FILETYPEMAP TEXT) (get-ext f)))

(defn xml-ext? [ext]
  (contains? (get FILETYPEMAP XML) ext))

(defn xml-file? [f]
  (contains? (get FILETYPEMAP XML) (get-ext f)))

(defn get-filetype [f]
  (let [ext (get-ext f)]
    (cond
      (text-ext? ext) :text
      (binary-ext? ext) :binary
      (archive-ext? ext) :archive
      (code-ext? ext) :code
      (xml-ext? ext) :xml
      :else :unknown)))

(defn unknown-file? [f]
  (= :unknown (get-filetype f)))

(defn from-name [name]
  (cond
    (= TEXT (lower-case name)) :text
    (= BINARY (lower-case name)) :binary
    (= ARCHIVE (lower-case name)) :archive
    (= CODE (lower-case name)) :code
    (= XML (lower-case name)) :xml
    :else :unknown))
