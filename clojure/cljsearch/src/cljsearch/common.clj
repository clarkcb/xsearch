(ns cljsearch.common)

(defn log-msg [msg & msgs]
  (println msg)
  (doseq [m msgs] (println m)))

(defn log-error [err]
  (log-msg (str "\nERROR: " err)))

(defn log-errors [errs]
  (doseq [e errs] (log-error e)))
