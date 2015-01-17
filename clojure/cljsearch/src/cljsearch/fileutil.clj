;;; ############################################################################
;;;
;;; fileutil.clj
;;;
;;; Utility functions for getting file extension and determining file type
;;;
;;; ############################################################################

(ns cljsearch
  #^{:author "Cary Clark",
     :doc "Module to provide file-related utility functions"}
  (:import (java.io File))
  (:use [clojure.set :only (union)])
  (:use [clojure.string :only (join split)])
  (:use [clojure.xml]))

;;; TODO: move to a config file
(def FILETYPESPATH "/Users/cary/src/git/xsearch/shared/filetypes.xml")

(defn get-filetypemap [file]
  (let [filetypes (filter #(= :filetype (:tag %)) (xml-seq (parse (File. file))))
        typenames (map :name (map :attrs filetypes))
        extension-nodes (map first (map :content (map first (map :content filetypes))))
        extension-sets (map #(set %) (map #(split % #"\s+") extension-nodes))
        filetypemap (zipmap typenames extension-sets)
        textmap (hash-map "text"
                  (union (get filetypemap "text")
                         (get filetypemap "code")
                         (get filetypemap "xml")))
        searchablemap (hash-map "searchable"
                        (union (get filetypemap "binary")
                               (get filetypemap "compressed")
                               (get filetypemap "text")))
        ]
  (merge filetypemap textmap searchablemap)))

(def FILETYPEMAP (get-filetypemap FILETYPESPATH))

(defn get-name
  [file]
  (if (or (instance? java.io.File file)
          (instance? java.util.zip.ZipEntry file)
          (instance? java.util.jar.JarEntry file))
    (.getName file)
    file))

(defn get-ext
  [file]
  (let [name (get-name file)]
    (if (> (.lastIndexOf name ".") 0)
      (.toLowerCase (last (split name #"\.")))
      "")))

(defn has-ext?
  [file ext]
  (= (.toLowerCase ext) (get-ext file)))

(defn binary-file? [file]
  (contains? (get FILETYPEMAP "binary") (get-ext file)))

(defn compressed-file? [file]
  (contains? (get FILETYPEMAP "compressed") (get-ext file)))

(defn searchable-file? [file]
  (contains? (get FILETYPEMAP "searchable") (get-ext file)))

(defn text-file? [file]
  (contains? (get FILETYPEMAP "text") (get-ext file)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn fileutil-test []
  (let [keyset (set (keys FILETYPEMAP))]

    ;; FILETYPEMAP assertions
    (println "FILETYPEMAP: " FILETYPEMAP)
    (assert (contains? keyset "binary"))
    (assert (contains? keyset "compressed"))
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

    ;; compressed-file? assertions
    (assert (not (compressed-file? "test.exe")))
    (assert (not (compressed-file? "test.txt")))
    (assert (compressed-file? "test.zip"))

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

(fileutil-test)

