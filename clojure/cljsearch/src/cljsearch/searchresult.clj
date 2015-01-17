;;; ############################################################################
;;;
;;; searchresult.clj
;;;
;;; Record and functionality for search results
;;;
;;; ############################################################################

(ns cljsearch.searchresult
  #^{:author "Cary Clark",
     :doc "Record and functionality for search results"})

; record to hold a search-result
(defrecord SearchResult [pattern file linenum line])
