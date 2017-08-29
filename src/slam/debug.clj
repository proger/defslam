(ns slam.debug)

(defn trace-header [message x]
  (println message)
  x)

(defn trace-pprint [message x]
  (print "=====" message "\n")
  (pprint x)
  (println "=====")
  x)

(defn traceln [x]
  (println x)
  x)
