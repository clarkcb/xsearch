(ns cljsearch.cljsearch
  (:gen-class)
  (:use [cljsearch.common :only (log-msg log-errors)]
        [cljsearch.searcher :only
          (search print-search-results print-matching-dirs print-matching-files
            print-matching-lines)]
        [cljsearch.searchoptions :only (settings-from-args usage)]))

(defn -main
  "This will be the main function for cljsearch"
  [& args]
  (let [[settings errs] (settings-from-args args)]
    (if (:debug settings) (log-msg settings))
    (if (empty? errs)
      (do
        (if (:printusage settings) (usage))
        (let [[results errs] (search settings)]
          (if (empty? errs)
            (do
              (if (:printresults settings) (print-search-results results settings))
              (if (:listdirs settings) (print-matching-dirs results))
              (if (:listfiles settings) (print-matching-files results))
              (if (:listlines settings) (print-matching-lines results settings)))
            (do
              (log-errors errs)
              (usage)))))
      (do
        (log-errors errs)
        (usage)))))
