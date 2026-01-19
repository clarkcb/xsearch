;;; ############################################################################
;;;
;;; searchsettings.clj
;;;
;;; Defines the settings for a given search instance
;;;
;;; ############################################################################

(ns cljsearch.searchsettings
  #^{:author "Cary Clark",
     :doc "Defines the settings for a given search instance"}
  (:use [clojure.set :only (union)]
        [clojure.string :as str :only (split)]
        [cljfind.consolecolor]
        [cljfind.filetypes :only (from-name)]
        [cljfind.findsettings]))

(defrecord SearchSettings
  [
    ^Boolean archives-only
    ^Boolean colorize
    ^Boolean debug
    ^String dir-color
    ^String ext-color
    ^String file-color
    ^Boolean first-match
    ^Boolean follow-symlinks
    ^clojure.lang.PersistentHashSet in-archive-extensions
    ^clojure.lang.PersistentHashSet in-archive-file-patterns
    ^clojure.lang.PersistentHashSet in-dir-patterns
    ^clojure.lang.PersistentHashSet in-extensions
    ^clojure.lang.PersistentHashSet in-file-patterns
    ^clojure.lang.PersistentHashSet in-file-types
    ^clojure.lang.PersistentHashSet in-lines-after-patterns
    ^clojure.lang.PersistentHashSet in-lines-before-patterns
    ^Boolean include-hidden
    ^String line-color
    ^Integer lines-after
    ^clojure.lang.PersistentHashSet lines-after-to-patterns
    ^clojure.lang.PersistentHashSet lines-after-until-patterns
    ^Integer lines-before
    ^Integer max-depth
    ^java.util.Date max-last-mod
    ^Integer max-line-length
    ^Integer max-size
    ^Integer min-depth
    ^java.util.Date min-last-mod
    ^Integer min-size
    ^Boolean multi-line-search
    ^clojure.lang.PersistentHashSet out-archive-extensions
    ^clojure.lang.PersistentHashSet out-archive-file-patterns
    ^clojure.lang.PersistentHashSet out-dir-patterns
    ^clojure.lang.PersistentHashSet out-extensions
    ^clojure.lang.PersistentHashSet out-file-patterns
    ^clojure.lang.PersistentHashSet out-file-types
    ^clojure.lang.PersistentHashSet out-lines-after-patterns
    ^clojure.lang.PersistentHashSet out-lines-before-patterns
    ^clojure.lang.PersistentHashSet paths
    ^Boolean print-dirs
    ^Boolean print-files
    ^Boolean print-lines
    ^Boolean print-matches
    ^Boolean print-results
    ^Boolean print-usage
    ^Boolean print-version
    ^Boolean recursive
    ^Boolean search-archives
    ^clojure.lang.PersistentHashSet search-patterns
    ^clojure.lang.Keyword sort-by
    ^Boolean sort-case-insensitive
    ^Boolean sort-descending
    ^String text-file-encoding
    ^Boolean unique-lines
    ^Boolean verbose
  ])

(def DEFAULT-SEARCH-SETTINGS
  (->SearchSettings
   false     ; archives-only
   true      ; colorize
   false     ; debug
   :cyan     ; dir-color
   :yellow   ; ext-color
   :magenta  ; file-color
   false     ; first-match
   false     ; follow-symlinks
   #{}       ; in-archive-extensions
   #{}       ; in-archive-file-patterns
   #{}       ; in-dir-patterns
   #{}       ; in-extensions
   #{}       ; in-file-patterns
   #{}       ; in-file-types
   #{}       ; in-lines-after-patterns
   #{}       ; in-lines-before-patterns
   false     ; include-hidden
   :green    ; line-color
   0         ; lines-after
   #{}       ; lines-after-to-patterns
   #{}       ; lines-after-until-patterns
   0         ; lines-before
   -1        ; max-depth
   nil       ; max-last-mod
   150       ; max-line-length
   0         ; max-size
   -1        ; min-depth
   nil       ; min-last-mod
   0         ; min-size
   false     ; multi-line-search
   #{}       ; out-archive-extensions
   #{}       ; out-archive-file-patterns
   #{}       ; out-dir-patterns
   #{}       ; out-extensions
   #{}       ; out-file-patterns
   #{}       ; out-file-types
   #{}       ; out-lines-after-patterns
   #{}       ; out-lines-before-patterns
   #{}       ; paths
   false     ; print-dirs
   false     ; print-files
   false     ; print-lines
   false     ; print-matches
   true      ; print-results
   false     ; print-usage
   false     ; print-version
   true      ; recursive
   false     ; search-archives
   #{}       ; search-patterns
   :filepath ; sort-by
   false     ; sort-case-insensitive
   false     ; sort-descending
   "utf-8"   ; text-file-encoding
   false     ; unique-lines
   false     ; verbose
   ))

(defn to-find-settings [^SearchSettings search-settings]
  (->FindSettings
   (:archives-only search-settings)             ; archives-only
   (:colorize search-settings)                  ; colorize
   (:debug search-settings)                     ; debug
   (:dir-color search-settings)                 ; dir-color
   (:ext-color search-settings)                 ; ext-color
   (:file-color search-settings)                ; file-color
   (:follow-symlinks search-settings)           ; follow-symlinks
   (:in-archive-extensions search-settings)     ; in-archive-extensions
   (:in-archive-file-patterns search-settings)  ; in-archive-file-patterns
   (:in-dir-patterns search-settings)           ; in-dir-patterns
   (:in-extensions search-settings)             ; in-extensions
   (:in-file-patterns search-settings)          ; in-file-patterns
   (:in-file-types search-settings)             ; in-file-types
   (:search-archives search-settings)           ; include-archives
   (:include-hidden search-settings)            ; include-hidden
   (:max-depth search-settings)                 ; max-depth
   (:max-last-mod search-settings)              ; max-last-mod
   (:max-size search-settings)                  ; max-size
   (:min-depth search-settings)                 ; min-depth
   (:min-last-mod search-settings)              ; min-last-mod
   (:min-size search-settings)                  ; min-size
   (:out-archive-extensions search-settings)    ; out-archive-extensions
   (:out-archive-file-patterns search-settings) ; out-archive-file-patterns
   (:out-dir-patterns search-settings)          ; out-dir-patterns
   (:out-extensions search-settings)            ; out-extensions
   (:out-file-patterns search-settings)         ; out-file-patterns
   (:out-file-types search-settings)            ; out-file-types
   (:paths search-settings)                     ; paths
   (:print-dirs search-settings)                ; print-dirs
   (:print-files search-settings)               ; print-files
   (:print-usage search-settings)               ; print-usage
   (:print-version search-settings)             ; print-version
   (:recursive search-settings)                 ; recursive
   (:sort-by search-settings)                   ; sort-by
   (:sort-case-insensitive search-settings)     ; sort-case-insensitive
   (:sort-descending search-settings)           ; sort-descending
   (:verbose search-settings)                   ; verbose
   ))

(defn need-lines-before [^SearchSettings settings]
  (or
   (> (:lines-before settings) 0)
   (not (empty? (:in-lines-before-patterns settings)))
   (not (empty? (:out-lines-before-patterns settings)))))

(defn need-lines-after [^SearchSettings settings]
  (or
   (> (:lines-after settings) 0)
   (not (empty? (:in-lines-after-patterns settings)))
   (not (empty? (:out-lines-after-patterns settings)))))

;(defn set-archives-only [settings b]
;  (let [with-search-archives (assoc settings :search-archives b)]
;    (if b
;      (assoc with-search-archives :archives-only true)
;      with-search-archives)))
