(ns cljsearch.config
  #^{:author "Cary Clark",
     :doc "Configuration values"}
  (:use [clojure.string :only (join)]))

(def HOME-NAME
  (if (.startsWith (System/getProperty "os.name") "Windows")
    "USERPROFILE"
    "HOME"))

(def HOME (System/getenv HOME-NAME))

(def SHAREDPATH
  (clojure.string/join java.io.File/separator [HOME "src" "git" "xsearch" "shared"]))

(def FILETYPESPATH
  (clojure.string/join java.io.File/separator [SHAREDPATH "filetypes.xml"]))

(def SEARCHOPTIONSPATH
  (clojure.string/join java.io.File/separator [SHAREDPATH "searchoptions.xml"]))
