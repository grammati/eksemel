(ns eksemel.test.xml
  (:require (eksemel
             [xml :as xml])
            (clojure
             [string :as string]))
  (:use clojure.test)
  (:import (org.xml.sax SAXParseException)))


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
          (is (empty? (xml/content elt))))))))

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
              (testing "attributes are always keywords"
                (let [e (nth elts 2)
                      a (xml/attrs e)]
                  (is (= 2 (count a)))
                  (is (= "value-1" (:attr-1 a)))
                  (is (= "value-2" (:a:attr-2 a))))))))))))


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
          (is (= "Processing Instruction after root " (:text (nth nodes 4)))))
        
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
              (let [e (second elts)
                    a (xml/attrs e)]
                (testing "namespaced attributes"
                  (is (= 2 (count a)))
                  (is (= "value-1" (:attr-1 a)))
                  (is (= "value-2" (get a [:attr-2 "xmlns-a"])))
                  (testing "original prefix is retained"
                    (is (= "a" (->> a keys (remove keyword?) first meta :prefix)))))))))))))

(deftest parsing-namespaces
  (testing "Multiple prefixes for same namespace"
    (let [xml "<foo xmlns:a='my-ns' xmlns:b='my-ns'><a:bar/><b:bar/></foo>"
          root (first (xml/parse xml))
          [c1 c2] (xml/content root)]
      (is (= {:xmlns {"a" "my-ns" "b" "my-ns"}} (meta root)))
      ;; Important: elements are equal, even though textually
      ;; different in the original XML.
      (is (= c1 c2))
      ;; But the original prefixes are retained
      (is (= "a" (-> c1 meta :prefix)))
      (is (= "b" (-> c2 meta :prefix)))
      )
    (let [xml "<foo xmlns:a='my-ns' xmlns:b='my-ns'><bar a:attr='value-1' b:attr='value-2'/></foo>"]
      ;; This XML is illegal with namespace-aware parsing on, because
      ;; the "same" attribute (semantically) appears twice.
      ;; Note: this should test for SAXParseException, but it's
      ;; actually wrapped in a RuntimeException. Oh well.
      (is (thrown? RuntimeException (xml/parse xml)))
      ;; With xmlns-awareness off, it's fine.
      (let [root (first (xml/parse xml {:xmlns-aware false}))
            a (-> root xml/content first xml/attrs)]
        (is (= 2 (count a)))))))

(deftest whitespace
  (let [xml (str "<foo>\n"
                   "  <bar>\n"
                   "    Hello World!\n"
                   "  </bar>\n"
                   "</foo>\n")]
    (testing "parsing with :whitespace option set to :keep"
      (let [t (filter string? (xml/flatten-nodes (xml/parse xml {:whitespace :keep})))]
        (is (= 3 (count t)))
        (is (= "\n  " (nth t 0)))
        (is (= "\n    Hello World!\n  " (nth t 1)))
        (is (= "\n" (nth t 2)))))
    (testing "parsing with :whitespace option set to :trim"
      (let [t (filter string? (xml/flatten-nodes (xml/parse xml {:whitespace :trim})))]
        (is (= 1 (count t)))
        (is (= "Hello World!" (first t)))))
    (testing "parsing with :whitespace option not explicitly set"
      (let [t (filter string? (xml/flatten-nodes (xml/parse xml)))]
        (is (= 1 (count t)))
        (is (= "\n    Hello World!\n  " (first t)))))
    (testing "parsing with :whitespace option set to :skip-blank gives default behavior"
      (let [t (filter string? (xml/flatten-nodes (xml/parse xml {:whitespace :skip-blank})))]
        (is (= 1 (count t)))
        (is (= "\n    Hello World!\n  " (first t)))))
    (testing "parsing with :whitespace set to a function calls it for each text node"
      (let [t (filter string? (xml/flatten-nodes (xml/parse xml {:whitespace #(-> % string/trim string/upper)})))]
        (is (= 3 (count t)))))
    ))

