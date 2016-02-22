(ns cljsearch.config
  #^{:author "Cary Clark",
     :doc "Configuration values"}
  (:require [clojure.java.io :as io])
  (:require [clojure.data.json :as json])
  (:use [clojure.string :only (join)]))

(def XSEARCHPATH
    (let [configjson (slurp (io/resource "config.json"))
          config (json/read-str configjson :key-fn keyword)]
      (config :xsearchpath)))

(def SHAREDPATH
  (clojure.string/join java.io.File/separator [XSEARCHPATH "shared"]))

(def FILETYPESPATH
  (clojure.string/join java.io.File/separator [SHAREDPATH "filetypes.xml"]))

(def SEARCHOPTIONSPATH
  (clojure.string/join java.io.File/separator [SHAREDPATH "searchoptions.xml"]))
