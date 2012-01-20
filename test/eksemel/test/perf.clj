(ns eksemel.test.perf
  (:require (eksemel [xml :as xml])
            (clojure.xml))
  (:import (javax.xml.parsers DocumentBuilderFactory))
  (:use clojure.test))



(defn parse-dom [f]
  (-> (DocumentBuilderFactory/newInstance)
      .newDocumentBuilder
      (.parse f)))
