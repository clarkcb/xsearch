(ns cljsearch.config
  #^{:author "Cary Clark",
     :doc "Configuration values"}
  (:use [clojure.string :as str :only (join)]))

(def HOME-NAME
  (if (.startsWith (System/getProperty "os.name") "Windows")
    "USERPROFILE"
    "HOME"))

(def HOME (System/getenv HOME-NAME))

(def XSEARCHPATH
  (str/join java.io.File/separator [HOME "src" "xsearch"]))

(def SHAREDPATH
  (str/join java.io.File/separator [XSEARCHPATH "shared"]))

(def FILETYPESPATH
  (str/join java.io.File/separator [SHAREDPATH "filetypes.xml"]))

(def SEARCHOPTIONSPATH
  (str/join java.io.File/separator [SHAREDPATH "searchoptions.xml"]))
