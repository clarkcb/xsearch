;;; ############################################################################
;;;
;;; fileutil.clj
;;;
;;; Utility functions for getting file extension and determining file type
;;;
;;; ############################################################################

(ns cljsearch.filetypes
  #^{:author "Cary Clark",
     :doc "Module to provide file-related utility functions"}
  (:import (java.io File))
  (:use [clojure.set :only (union)])
  (:use [clojure.string :only (split)])
  (:use [clojure.xml])
  (:use [cljsearch.fileutil])
  )

;;; TODO: move to a config file
(def FILETYPESPATH "~/src/git/xsearch/shared/filetypes.xml")

(defn get-filetypemap [f]
  (let [ftfile (File. (expand-path FILETYPESPATH))
        filetypes (filter #(= :filetype (:tag %)) (xml-seq (parse ftfile)))
        typenames (map :name (map :attrs filetypes))
        extension-nodes (map first (map :content (map first (map :content filetypes))))
        extension-sets (map #(set %) (map #(split % #"\s+") extension-nodes))
        filetypemap (zipmap typenames extension-sets)
        textmap (hash-map "text"
                  (union (get filetypemap "text")
                         (get filetypemap "code")
                         (get filetypemap "xml")))
        searchablemap (hash-map "searchable"
                        (union (get filetypemap "archive")
                               (get filetypemap "binary")
                               (get filetypemap "text")))
        ]
  (merge filetypemap textmap searchablemap)))

(def FILETYPEMAP (get-filetypemap FILETYPESPATH))

(defn binary-file? [f]
  (contains? (get FILETYPEMAP "binary") (get-ext f)))

(defn archive-file? [f]
  (contains? (get FILETYPEMAP "archive") (get-ext f)))

(defn searchable-file? [f]
  (contains? (get FILETYPEMAP "searchable") (get-ext f)))

(defn text-file? [f]
  (contains? (get FILETYPEMAP "text") (get-ext f)))

(defn get-filetype [f]
  (cond
    (text-file? f) :text
    (binary-file? f) :binary
    (archive-file? f) :archive
    :else :unknown))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn filetypes-test []
  (let [keyset (set (keys FILETYPEMAP))]

    ;; FILETYPEMAP assertions
    (println "FILETYPEMAP: " FILETYPEMAP)
    (assert (contains? keyset "archive"))
    (assert (contains? keyset "binary"))
    (assert (contains? keyset "searchable"))
    (assert (contains? keyset "text"))

    ;; get-name assertions
    (assert (= "test.exe" (get-name "test.exe")))
    (assert (= "test.exe" (get-name (File. "test.exe"))))

    ;; get-ext assertions
    (assert (= "exe" (get-ext "test.exe")))
    (assert (= "exe" (get-ext (File. "test.exe"))))

    ;; binary-file? assertions
    (assert (binary-file? "test.exe"))
    (assert (not (binary-file? "test.txt")))
    (assert (not (binary-file? "test.zip")))

    ;; archive-file? assertions
    (assert (not (archive-file? "test.exe")))
    (assert (not (archive-file? "test.txt")))
    (assert (archive-file? "test.zip"))

    ;; searchable-file? assertions
    (assert (searchable-file? "test.exe"))
    (assert (searchable-file? "test.txt"))
    (assert (searchable-file? "test.zip"))
    (assert (not (searchable-file? "test.ZZZ")))

    ;; text-file? assertions
    (assert (not (text-file? "test.exe")))
    (assert (text-file? "test.txt"))
    (assert (not (text-file? "test.zip")))
    )
  )

;(filetypes-test)

