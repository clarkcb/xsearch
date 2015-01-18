(ns cljsearch.cljsearch
  (:gen-class)
  (:use cljsearch.common)
  (:use cljsearch.searcher)
  (:use cljsearch.searchoptions)
  (:use cljsearch.searchsettings)
)

(defn -main
  "This will be the main function for cljsearch"
  [& args]
  (let [[settings errs] (settings-from-args DEFAULT-SETTINGS args [])]
    (if (empty? errs)
      (do
        (if (:debug settings) (log-msg settings))
        (if (:printusage settings) (usage))
        (search settings)
        (if (:printresults settings) (print-search-results))
        (if (:listdirs settings) (print-matching-dirs))
        (if (:listfiles settings) (print-matching-files))
        (if (:listlines settings) (print-matching-lines settings))
      )
      (do
        (log-errors errs)
        (usage)
      )
    )
  )
)
