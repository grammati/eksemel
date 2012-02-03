(ns eksemel.test.perf
  (:require (eksemel [xml :as xml])
            clojure.xml
            (yoodls [bench :as b]))
  (:import (javax.xml.parsers DocumentBuilderFactory))
  (:use clojure.test))


(defmacro bench [& forms]
  `(time ~@forms))

(defn parse-dom [f]
  (-> (DocumentBuilderFactory/newInstance)
      .newDocumentBuilder
      (.parse f)
      .getDocumentElement))


(deftest parse-bench
  (let [f "test/resources/sample-1K-records.xml"]
    (slurp f) ; force it into os cache, to get disk-access out of the timing
    (bench (last (xml/sax-seq f)))
    (bench (last (-> f
                     xml/sax-seq
                     xml/throw-on-error
                     ;xml/merge-adjacent-text
                     (xml/normalize-text :skip-blank))))
    (bench (xml/parse f xml/old-parse-options))
    #_(bench (xml/parse f xml/default-parse-options))
    (bench (clojure.xml/parse f))))