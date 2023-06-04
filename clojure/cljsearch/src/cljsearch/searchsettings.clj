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
        [cljfind.filetypes :only (from-name)]
        [cljfind.findsettings :only (->FindSettings)]))

(defrecord SearchSettings
  [
    archives-only
    colorize
    debug
    exclude-hidden
    first-match
    in-archive-extensions
    in-archive-file-patterns
    in-dir-patterns
    in-extensions
    in-file-patterns
    in-file-types
    in-lines-after-patterns
    in-lines-before-patterns
    lines-after
    lines-after-to-patterns
    lines-after-until-patterns
    lines-before
    list-dirs
    list-files
    list-lines
    max-last-mod
    max-line-length
    max-size
    min-last-mod
    min-size
    multi-line-search
    out-archive-extensions
    out-archive-file-patterns
    out-dir-patterns
    out-extensions
    out-file-patterns
    out-file-types
    out-lines-after-patterns
    out-lines-before-patterns
    paths
    print-results
    print-usage
    print-version
    recursive
    search-archives
    search-patterns
    sort-by
    sort-case-insensitive
    sort-descending
    text-file-encoding
    unique-lines
    verbose
  ])

(def DEFAULT-SETTINGS
  (->SearchSettings
   false     ; archives-only
   true      ; colorize
   false     ; debug
   true      ; exclude-hidden
   false     ; first-match
   #{}       ; in-archive-extensions
   #{}       ; in-archive-file-patterns
   #{}       ; in-dir-patterns
   #{}       ; in-extensions
   #{}       ; in-file-patterns
   #{}       ; in-file-types
   #{}       ; in-lines-after-patterns
   #{}       ; in-lines-before-patterns
   0         ; lines-after
   #{}       ; lines-after-to-patterns
   #{}       ; lines-after-until-patterns
   0         ; lines-before
   false     ; list-dirs
   false     ; list-files
   false     ; list-lines
   nil       ; max-last-mod
   150       ; max-line-length
   0         ; max-size
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
   (:debug search-settings)                     ; debug
   (:exclude-hidden search-settings)            ; exclude-hidden
   (:search-archives search-settings)           ; include-archives
   (:in-archive-extensions search-settings)     ; in-archive-extensions
   (:in-archive-file-patterns search-settings)  ; in-archive-file-patterns
   (:in-dir-patterns search-settings)           ; in-dir-patterns
   (:in-extensions search-settings)             ; in-extensions
   (:in-file-patterns search-settings)          ; in-file-patterns
   (:in-file-types search-settings)             ; in-file-types
   (:list-dirs search-settings)                 ; list-dirs
   (:list-files search-settings)                ; list-files
   (:max-last-mod search-settings)              ; max-last-mod
   (:max-size search-settings)                  ; max-size
   (:min-last-mod search-settings)              ; min-last-mod
   (:min-size search-settings)                  ; min-size
   (:out-archive-extensions search-settings)    ; out-archive-extensions
   (:out-archive-file-patterns search-settings) ; out-archive-file-patterns
   (:out-dir-patterns search-settings)          ; out-dir-patterns
   (:out-extensions search-settings)            ; out-extensions
   (:out-file-patterns search-settings)         ; out-file-patterns
   (:out-file-types search-settings)            ; out-file-types
   (:paths search-settings)                     ; paths
   (:print-usage search-settings)               ; print-usage
   (:print-version search-settings)             ; print-version
   (:sort-by search-settings)                   ; sort-by
   (:sort-case-insensitive search-settings)     ; sort-case-insensitive
   (:sort-descending search-settings)           ; sort-descending
   (:recursive search-settings)                 ; recursive
   (:verbose search-settings)                   ; verbose
   ))

(defn add-element [x coll]
  (conj coll x))

(defn add-extensions [settings exts extname]
  (if (empty? exts)
    settings
    (add-extensions
      (update-in settings [extname] #(add-element (first exts) %)) (rest exts) extname)))

(defn add-extension [settings ext extname]
  (let [t (type ext)]
    (cond
      (= t (type []))
        (add-extensions settings ext extname)
      :else
        (add-extensions settings (str/split ext #",") extname))))

(defn add-file-types [settings types typesname]
  (if (empty? types)
    settings
    (add-file-types
     (update-in settings [typesname] #(add-element (from-name (first types)) %)) (rest types) typesname)))

(defn add-file-type [settings typ typesname]
  (let [t (type typ)]
    (cond
      (= t (type []))
      (add-file-types settings typ typesname)
      :else
      (add-file-types settings (str/split typ #",") typesname))))

(defn add-paths [settings paths]
  (if (empty? paths)
    settings
    (add-paths
      (update-in settings [:paths] #(add-element (first paths) %)) (rest paths))))

(defn add-path [settings path]
  (let [t (type path)]
    (cond
      (= t (type []))
        (add-paths settings path)
      :else
        (add-paths settings [path]))))

(defn add-patterns [settings pats patname]
  (if (empty? pats)
    settings
    (add-patterns
      (update-in settings [patname] #(add-element (re-pattern (first pats)) %)) (rest pats) patname)))

(defn add-pattern [settings p patname]
  (let [t (type p)]
    (cond
      (= t (type []))
        (add-patterns settings p patname)
      :else
        (add-patterns settings [p] patname))))

(defn set-num [settings n numname]
  (let [t (type n)]
    (cond
      (= t java.lang.Long)
        (assoc settings numname n)
      :else
        (assoc settings numname (read-string n)))))

(defn set-archives-only [settings b]
  (let [with-search-archives (assoc settings :search-archives b)]
    (if b
      (assoc with-search-archives :archives-only true)
      with-search-archives)))

(defn set-debug [settings b]
  (let [with-debug (assoc settings :debug true)]
    (if b
      (assoc with-debug :verbose true)
      with-debug)))
