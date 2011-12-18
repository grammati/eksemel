(ns eksemel.test.xml
  (:require [eksemel
             [xml :as xml]]
            [clojure
             [string :as string]])
  (:use [clojure.test]))


(defn xml-str=
  "Compare two XML strings, ignoring whitespace."
  [a b]
  (letfn [(squish [x]
            (-> x
                string/trim
                (string/replace #">\s+<" "><")))]
    (= (squish a) (squish b))))

(defn xml=
  "Compare two XML objects."
  [a b]
  (try
    (= (xml/parse a) (xml/parse b))
    (catch Exception x
      (xml-str= a b))))


;;; Parsing
(deftest parse-sources
  (testing "parse works with a file, URL, string, etc..."
    (doseq [source ["./test/resources/sample-1.xml"
                    "https://raw.github.com/grammati/eksemel/master/test/resource/sample-1.xml"
                    (-> "<foo/>" java.io.StringReader.)
                    (-> "<foo/>" java.io.StringReader. org.xml.sax.InputSource.)
                    (-> "<foo/>" (.getBytes (java.nio.charset.Charset/forName "utf-8")))
                    (-> "<foo/>" (.getBytes (java.nio.charset.Charset/forName "utf-8"))
                                 java.io.ByteArrayInputStream.)
                    ;; Can also parse a string directly (but I may change it to
                    ;; parse-str instead - can a file name ever start with "<"?).
                    "<foo/>"]]
      (let [parsed (xml/parse source)]
        (is (= 1 (count parsed)))
        (is (xml/element? (first parsed)))
        (is (= :foo (xml/tag (first parsed))))
        (is (nil? (xml/uri (first parsed))))
        ))))

(def xml-1 "<a><b/></a>")
(def xml-2 "<a a1='v1'><b a2='v2'/></a>")

(defn round-trip [xml]
  (xml/emit (xml/parse xml)))

(deftest parse-no-namespaces
  (is (= xml-1 (round-trip xml-1)))
  (is (= xml-2 (round-trip xml-2))))