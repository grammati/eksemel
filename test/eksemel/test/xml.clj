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
                    "https://raw.github.com/grammati/eksemel/master/test/resources/sample-1.xml"
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
        (let [elt (first parsed)]
          (is (xml/element? elt))
          (is (= :foo (xml/tag elt)))
          (is (nil? (xml/uri elt)))
          (is (empty? (xml/attrs elt)))
          (is (empty? (xml/content elt))))
        ))))

(deftest parse-with-old-defaults
  (let [path "./test/resources/kitchen-sink-1.xml"]
    (testing "Clojure.xml-style parsing options (no namespaces, comments, etc.)"
      (let [nodes (xml/parse path xml/old-parse-options)]
        
        (testing "There is a single root"
          (is (= 1 (count nodes))))
        
        (let [root (first nodes)]
          
          (testing "The root is an Element"
            (is (xml/element? root)))
          
          (testing "Comments and processing-instructions are discarded"
            (is (every? #(or (xml/element? %) (string? %)) (xml/content root))))
          
          (let [elts (filter xml/element? (xml/content root))
                text (filter string? (xml/content root))]
            
            (testing "Whitespace-only text nodes are discarded"
              (is (not-any? string/blank? text)))

            (testing "CDATA is treated as a regular text node"
              (is (= "hello cdata! <markup> allowed here &\"'><" (string/trim (first text)))))

            (testing "Whitespace is kept (not trimmed) in text nodes"
              (is (= "\n  Hello XML\n  " (second text))))
            
            (testing  "xmlns-awareness is turned off"
              (testing "elements have a nil uri"
                (is (nil? (xml/uri root)))
                (is (every? nil? (map xml/uri elts))))
              (testing "xml namespace declarations are treated as normal attributes"
                (is (= 2 (count (xml/attrs root))))
                (is (= "xmlns-default" (get-in root [:attrs :xmlns])))
                (is (= "xmlns-a" (get-in root [:attrs :xmlns:a]))))
              (testing "undeclared prefixes are allowed"
                (let [elt (first elts)]
                  (is (= :undeclared:foo (xml/tag elt)))
                  (is (= [:junk:attr "xxx"] (first (xml/attrs elt))))))
              (testing "declared prefixes are allowed"
                (is (= :a:foo (xml/tag (second elts)))))
              
              )))))))


(deftest parse-with-new-defaults
  (let [path "./test/resources/kitchen-sink-2.xml"]
    (testing "New-style parsing options (namespaces, comments, etc.)"
      (let [nodes (xml/parse path)]
        
        (testing "There are multiple roots"
          (is (= 5 (count nodes))))

        (testing "Comments and processing instructions can be before or after the root Element"
          (is (xml/comment? (nth nodes 0)))
          (is (xml/processing-instruction? (nth nodes 1)))
          (is (xml/comment? (nth nodes 3)))
          (is (xml/processing-instruction? (nth nodes 4))))

        (testing "Comment text is retained"
          (is (= " Comment before root " (:text (nth nodes 0))))
          (is (= " Comment after root " (:text (nth nodes 3)))))

        (testing "Processing instructions are kept"
          (is (= "test" (:target (nth nodes 1))))
          (is (= "Processing Instruction before root " (:text (nth nodes 1))))
          (is (= "test" (:target (nth nodes 4))))
          (is (= "Processing Instruction after root " (:text (nth nodes 4))))
          )
        
        (let [root (nth nodes 2)]
          
          (testing "The root element is an Element"
            (is (xml/element? root)))
          
          (let [children (xml/content root)
                elts (filter xml/element? children)
                text (filter string? children)]

            (testing "Comments inside an element are kept"
              (let [c (nth children 0)]
                (is (xml/comment? c))
                (is (= " Comment child of root " (:text c)))))

            (testing "Processing instructions inside an element are kept"
              (let [pi (nth children 1)]
                (is (xml/processing-instruction? pi))
                (is (= "foobar" (:target pi)))
                (is (= "Processing Instruction child of root " (:text pi)))))
            
            (testing "Whitespace-only text nodes are discarded"
              (is (not-any? string/blank? text)))

            (testing "CDATA is stored in a CData instance"
              (let [cd (nth children 2)]
                (is (xml/cdata? cd))
                (is (= "hello cdata! <markup> allowed here &\"'><" (:text cd)))))

            (testing "Whitespace is kept (not trimmed) in text nodes"
              (is (= "\n  Hello XML\n  " (first text))))
            
            (testing  "xmlns-awareness is turned on"
              (testing "elements have a uri"
                (is (= "xmlns-default" (xml/uri root)))
                (is (not-any? nil? (map xml/uri elts))))
              (testing "xml namespace declarations are not stored as attributes"
                (is (empty? (xml/attrs root))))
              (let [e (first elts)]
                (testing "prefixes are resolved to uris"
                  (is (= :foo (xml/tag e)))
                  (is (= "xmlns-a" (xml/uri e))))
                (testing "the original prefix is retained"
                  (is (= "a" (-> e meta :prefix))))
                (testing "the prefix-to-namespace mappings are retained"
                  (is (= {"" "xmlns-default" "a" "xmlns-a"} (-> e meta :xmlns)))))
              
              )))))))
