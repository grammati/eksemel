(ns
    ^{:doc "XML"
      :author "Chris Perkins"}
  eksemel.xml
  (:require (clojure [string :as  string])
            (clojure.java [io :as io])
            (yoodls [pipe :as pipe]))
  (:import [java.io Writer Reader StringReader]
           [org.xml.sax Attributes InputSource Locator]
           [org.xml.sax.ext DefaultHandler2]
           [org.xml.sax.helpers XMLReaderFactory]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protocols
(defprotocol ElementAccessor
  (tag [this] "Return the tag of the element")
  (attrs [this] "Return a map of the attributes of the element")
  (content [this] "Return a sequence of children of the element"))

(defprotocol XmlNamespaced
  (uri [this] "Return the namespace URI of the element")
  (prefix [this] "Returns the prefix that should be used when emitting the element. Can return nil to indicate no preference."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Records for XML node types

(defrecord Element [tag attrs content uri]
  
  ElementAccessor
  (tag [this] tag)
  (attrs [this] attrs)
  (content [this] content)
  
  XmlNamespaced
  (uri [this] uri)
  (prefix [this]
    (when-let [md (meta this)]
      (get-in md [:xmlns uri]))))

(defrecord Comment [^String text])

(defrecord CData [^String text])

(defrecord PI [^String target ^String text])


(defn element
  "Factory function to return an element."
  ([tag]                   (Element. tag nil   nil     nil))
  ([tag attrs]             (Element. tag attrs nil     nil))
  ([tag attrs content]     (Element. tag attrs content nil))
  ([tag attrs content uri] (Element. tag attrs content uri)))

(defn element?
  "Returns true if the given object is an element."
  [e]
  (instance? Element e))

(defn comment? [c] (instance? Comment c))
(defn processing-instruction? [pi] (instance? PI pi))
(defn cdata? [cd] (instance? CData cd))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
;; TODO - clean up, docstrings, etc.

(defn get-attr [elt attr]
  (get-in elt [:attrs attr]))

(defn assoc-attr [elt & attr-vals]
  (assoc elt :attrs (apply assoc (:attrs elt) attr-vals)))

(defn merge-attrs [elt new-attrs]
  (assoc elt :attrs (merge (:attrs elt) new-attrs))) ;TODO - skip nils

(defn dissoc-attr [elt & attrs]
  (assoc elt :attrs (apply dissoc (:attrs elt) attrs)))

(defn pop-attr [elt attr]
  [(dissoc-attr elt attr) (get-attr elt attr)])

(defn pop-attrs [elt & attrs]
  (loop [elt elt
         [attr & attrs] attrs
         vals []]
    (if attr
      (let [[elt val] (pop-attr elt attr)]
        (recur elt attrs (conj vals val)))
      [elt vals])))

(defn append-content [elt & children]
  (update-in elt [:content] concat children))

(defn flatten-element [elt]
  (tree-seq element? content elt))

(defn flatten-elements [elts]
  (mapcat flatten-element elts))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing XML

(defn- parse-error [& messages]
  (throw (IllegalArgumentException. (apply str messages))))


(defn attrs->map
  [^Attributes attrs xmlns-aware]
  (let [attr-key
        (if xmlns-aware
          (fn [^Attributes attrs ^Long i]
            (let [uri (.getURI attrs i)
                  q-name (.getQName attrs i)]
              (if (.isEmpty uri)
                (keyword q-name)
                (let [[prefix _] (string/split q-name #":" 2)]
                  (with-meta
                    [(keyword (.getLocalName attrs i)) uri]
                    {:prefix prefix})))))
          (fn [^Attributes attrs ^Long i]
            (keyword (.getQName attrs i))))]
    (into {} (for [^Long i (range (.getLength attrs))]
               [(attr-key attrs i) (.getValue attrs i)]))))


(defn sax-handler
  "Return an instance of org.xml.sax.ext.DefaultHandler2 that will
  handle SAX events by calling the supplied function, passing a vector
  of 1 or more items, the first being the event type as a keyword, and
  the others being the associated data.

  The event types and their associated data are:
    [:start-element uri local-name q-name attrs]
    [:end-element uri local-name q-name]
    [:start-prefix-mapping prefix uri]
    [:end-prefix-mapping prefix]
    [:text string]
    [:start-cdata]
    [:end-cdata]
    [:processing-instruction target data]
    [:comment text]
  "
  [f & [opts]]
  (let [{:keys [xmlns-aware line-numbers]} opts
        locator (atom nil)
        with-line-numbers (fn [evt]
                            (if-let [^Locator loc @locator]
                              (with-meta
                                evt
                                {:line (.getLineNumber loc)
                                 :col  (.getColumnNumber loc)})
                              evt))
        f (if line-numbers
            #(f (with-line-numbers %))
            f)
        nil-if-empty (fn [^String s] (if (pos? (.length s)) s))]
    
    (proxy [DefaultHandler2] []

      ;; For getting line & column numbers
      (setDocumentLocator [^Locator loc]
        (reset! locator loc))
      
      ;; Elements
      (startElement [uri local-name q-name attrs]
        (f [:start-element (nil-if-empty uri) local-name q-name (attrs->map attrs xmlns-aware)]))
      (endElement [uri local-name q-name]
        (f [:end-element (nil-if-empty uri) local-name q-name]))

      ;; Text
      (characters [^chars ch ^long start ^long length]
        (f [:text (String. ch start length)]))
    
      ;; As far as I can tell, ignorableWhitespace never gets called.
      ;; does anyone ever need this?
      #_(ignorableWhitespace [^chars ch ^long start ^long length]
          (f [:whitespace (String. ch start length)]))
    
      ;; Keep track of which text came from a CDATA section
      (startCDATA []
        (f [:start-cdata]))
      (endCDATA []
        (f [:end-cdata]))

      ;; Processing Instructions
      (processingInstruction [target data]
        (f [:processing-instruction target data]))

      ;; Mapping prefixes to namespace URIs
      (startPrefixMapping [prefix uri]
        (f [:start-prefix-mapping prefix uri]))
      (endPrefixMapping [prefix]
        (f [:end-prefix-mapping prefix]))

      ;; Comments
      (comment [^chars ch ^long start ^long length]
               (f [:comment (String. ch start length)]))
    
      )))


(defprotocol AsInputSource
  (as-input-source [this] "Wrap this object as an instance of org.xml.sax.InputSource"))

(extend-protocol AsInputSource
  String
  (as-input-source [s]
    (let [^Reader r (if (.startsWith s "<") ;; too "clever"? ... <shrug>
                      (StringReader. s)
                      (io/reader s))]
      (InputSource. r)))

  InputSource
  (as-input-source [this] this)
  
  Object
  (as-input-source [o]
    (InputSource. (io/reader o))))


(defn sax-seq
  "Given a source of XML, returns a sequence of events, in the form of
   vectors of [type data ...]. See sax-handler for details."
  [in & [opts]]
  (let [{:keys [xmlns-aware]} opts
        ^InputSource in (as-input-source in)
        parser          (XMLReaderFactory/createXMLReader) ;TODO - make this pluggable
        [put event-seq] (pipe/pipe {:capacity 4096})
        handler         (sax-handler put opts)]
    
    (.setContentHandler parser handler)
    
    (when xmlns-aware
      ;; Set the "lexical-handler" property in order to get
      ;; start-prefix-mapping & end-prefix-mapping events.
      (.setProperty parser
                    "http://xml.org/sax/properties/lexical-handler"
                    handler))
    
    ;; Setting this to true means that undeclared namespace-prefixes
    ;; will be an error.
    (.setFeature parser
                 "http://xml.org/sax/features/namespaces"
                 (if xmlns-aware true false))

    ;; Run the SAX parser in a thread.
    (future
      (try
        (.parse parser in)
        (catch Exception e
          (put [:error e]))
        (finally
         (put nil))))

    event-seq))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Transformations of event streams

(defn throw-on-error [events]
  (map (fn [[type exc :as evt]]
         (if (= :error type)
           (throw exc)
           evt))
       events))


(defn merge-adjacent-text
  "Transform the event stream so that adjacent bits of text are
   emitted as a single text event."
  [events]
  (letfn [(text? [[type]]
            (= :text type))
          (merge-if-text [[e :as  evts]]
            (if (text? e)
              (list [:text (apply str (map second evts))])
              evts))]
    (->> events
         (partition-by text?)
         (mapcat merge-if-text))))


(defn skip-whitespace
  "Filters out whitespace-only text events."
  [events]
  (remove (fn [[type s :as evt]]
            (and (= type :text)
                 (string/blank? s)))
          events))

(defn ignore
  "Filters out events of the given types."
  [events & event-types]
  (let [ignored-types (set event-types)
        ignorable? (fn [[type]] (ignored-types type))]
    (remove ignorable? events)))


(defn events->nodes
  "Takes a seq of SAX events, and returns a 2-element vector of:
   1 - A seq of nodes extracted from the event seq, and
   2 - A seq of the unconsumed events.
  "
  [events & [opts]]
  ;; TODO - this function is way too busy
  (let [{:keys [xmlns-aware]} opts

        ;; Memoize the metadata maps
        element-metadata
        (memoize
          (fn [md-map prefix]
            (if prefix
              (assoc md-map :prefix prefix)
              md-map)))
        
        make-element
        (if xmlns-aware
          (fn [[_ uri local-name q-name attrs] content md-map]
            (let [[prefix tag] (string/split q-name #":" 2)
                  prefix (if tag prefix nil)]
              (with-meta
                (Element. (keyword local-name)
                          attrs
                          content
                          uri)
                (element-metadata md-map prefix))))
          (fn [[_ _ _ q-name attrs] content _]
            (Element. (keyword q-name)
                      attrs
                      content
                      nil)))
        
        make-nodes
        (fn make-nodes [events md-map]
          (loop [nodes []
                 [[type data :as evt] & events] events
                 md-stack (list md-map)]
            
            (case type
              
              :start-prefix-mapping
              (let [[_ prefix uri] evt
                    md-map (assoc-in (peek md-stack) [:xmlns prefix] uri)]
                (recur nodes events (conj md-stack md-map)))

              :end-prefix-mapping
              ;; TODO - sanity check? make sure the prefix is in the md-map?
              (recur nodes events (pop md-stack))
              
              :start-element
              (let [md-map (peek md-stack)
                    [children events] (make-nodes events md-map)
                    elt (make-element evt children md-map)]
                (recur (conj nodes elt)
                       events
                       md-stack))

              :text
              (recur (conj nodes data)
                     events
                     md-stack)

              :comment
              (recur (conj nodes (Comment. data))
                     events
                     md-stack)

              :start-cdata
              (let [[text-nodes [end-cdata & events]]
                    (split-with (fn [[type]] (= type :text)) events)]
                (assert (= (end-cdata 0) :end-cdata) "Expected :end-cdata event.")
                (recur (conj nodes (CData. (apply str (map second text-nodes))))
                       events
                       md-stack))

              :processing-instruction
              (let [[_ target text] evt]
                (recur (conj nodes (PI. target text))
                       events
                       md-stack))

              (:end-element nil)
              [nodes events]
              
              )))]
    (make-nodes events nil)))

(defn make-tree
  "Takes a seq of SAX events and returns a vector of the top-level nodes."
  [events & [opts]]
  (let [[nodes events] (events->nodes events opts)]
    (when (seq events)
      ;; This shouldn't be possible if the source of the events is an
      ;; actual SAX parser, but could happen for an event-seq from
      ;; some other source.
      (parse-error "Unconsumed SAX events: " events))
    nodes))

;; New default parse options.
(def default-parse-options
  {:xmlns-aware             true
   :keep-whitespace         false
   :comments                true
   :cdata                   true
   :processing-instructions true
   :line-numbers            false
   })

;; For backward-compatibility. Options for parsing clojure.xml-style.
(def old-parse-options
  {:xmlns-aware             false
   :keep-whitespace         false
   :comments                false
   :cdata                   false
   :processing-instructions false
   :line-numbers            false
   })

(def ^{:dynamic true
       :doc "Map of parsing options. Any options not explitly provided
  in a call to parse will take their values from this map."}
  *parse-options* default-parse-options)

(defn parse
  "Takes something convertable to a InputSource, and returns a seq
   of top-level XML nodes.

   The opts parameter is an optional map of options. The defaults for
   all options are in the map *parse-options*, which is rebindable.

     :keep-whitespace
       If true, whitespace-only text nodes are kept, othewise they
       are discarded.

     :comments
     :cdata
     :processing-instructions
       If false, nodes of the corresponding type are discarded.

     :line-numbers
       If true, the parser will put line-numbers and column-numbers
       in the metadata of elements, when supported by the SAX parser.

     :xmlns-aware
       Sets namespace-awareness in the SAX parser. See below.

   Namespace-awareness has the following effects:

   When set to false, XML namespaces are more-or-less ignored, and
   tags and attributes are stored as they appear textually in the
   XML, including prefixes.

   When set to true, namespace URIs are stored as part of Elements'
   values, and prefix information is stored in metadata.

   Specifically, setting this when parsing causes the following
   differences in the resulting tree, where each point below lists the
   effects of setting :xmlns-aware to true (t) or false (f):

    * xmlns declarations:
       t: stored in metadata as :xmlns-decls
       f: present in the attrs map, like regular attributes
        
    * The ns field of Elements:
       t: the namespace URI (or nil, for the default namespace)
       f: always nil

    * The tag field of Elements:
       t: the localName only (no prefix)
       f: the QName (prefix:tagname)

    * Unprefixed attributes:
       t: attribute name as a keyword
       f: attribute name as a keyword

    * Prefixed (namespaced) attributes:
       t: vector of [:localName ns-uri], with {:prefix prefix} metadata
       f: the QName as a keyword (prefix:attrname)

    * Other stuff that I can't remember right now:
       t: ???
       f: ???

   "
  
  [source & [opts]]
  
  ;; This works by producing a seq of SAX-events, and then running it
  ;; through a series of transformations. This should allow quite a
  ;; bit flexibility in deciding exactly which transformations to
  ;; apply, at the cost of being slightly less efficient - for
  ;; example, events for comments, processing-instructions, etc. may
  ;; be produced by the parser and added to the seq, but then
  ;; ignored. This inefficiency should be ameliorated somewhat by the
  ;; fact that those types of nodes are relatively rare in XML.
  
  (let [;; Use defaults for options unless overridden.
        opts (merge *parse-options* opts)
        {:keys [xmlns-aware
                comments cdata processing-instructions
                keep-whitespace]} opts

        ;; First, run the SAX parser to produce the full sequence of
        ;; events. Give it the nice short name "e", since it will be
        ;; repeated a lot.
        e (sax-seq source opts)

        ;; Skip ignorable events.
        e (let [ignored-types
                (reduce into nil
                        [(when-not comments [:comment])
                         (when-not cdata [:start-cdata :end-cdata])
                         (when-not processing-instructions [:processing-instruction])
                         ;; Maybe don't need this line (the parser
                         ;; won't emit these anyway, right?):
                         (when-not xmlns-aware [:start-prefix-mapping :end-prefix-mapping])
                         ])]
            (if ignored-types (apply ignore e ignored-types) e))
        
        ;; Since the SAX parser runs in another thread, parsing errors
        ;; are caught and put into the event-seq as [:error
        ;; <exception>].  This transformation will re-throw them.
        e (throw-on-error e)

        ;; SAX parsers can emit multiple "characters" events for one
        ;; piece of text (for example, when it contains entities, such
        ;; as &amp;). It's generally more convenient to work with
        ;; adjacent text nodes merged into one.
        e (merge-adjacent-text e)

        ;; Optionally discard whitespace-only nodes
        e (if keep-whitespace e (skip-whitespace e))

        ]

    ;; Finally, turn the resulting event-seq into a tree (almost) of
    ;; nodes. This actually returns all the top-level nodes, which
    ;; should consist of exactly one Element, as well as any number of
    ;; Comments and PIs preceeding and/or following the Element.
    (make-tree e opts)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emitting XML

(def ^:dynamic *emitter-state* nil)

(defn update-emitter-state [elt]
  )

(defn valid-prefix?
  "Returns true if the given prefix is valid for the uri, for the current value
   of *emitter-state*."
  [prefix uri]
  (let [declared-prefixes (get-in *emitter-state* [:prefixes uri])]
    (some #(= prefix %) declared-prefixes)))


(defprotocol XMLPrefix
  (prefix-for [this] "Returns the prefix to use for "))

(extend-protocol XMLPrefix
  
  Element
  (prefix-for [elt]
    (if-let [uri (uri elt)]
      (or (:prefix (meta elt))
          (peek (get-in *emitter-state* [:prefixes uri])))))

  ;; Namespaced attribute
  clojure.lang.IPersistentVector
  (prefix-for [[name uri :as attr]]
    ())

  ;; Non-namespaced attribute
  clojure.lang.Keyword
  (prefix-for [_]
    nil)
  )


(defprotocol XMLWritable
  (emit-xml [this ^Writer out]))

(defprotocol XMLWritableAttr
  (emit-attr [this value ^Writer out]))

(defn ^String xml-escape [^String s]
  (-> s
      (.replaceAll "&"  "&amp;")
      (.replaceAll "<"  "&lt;")))

(defn ^String xml-escape-attr [^String s]
  (-> s
      xml-escape
      (.replaceAll "'"  "&apos;")))

(extend-protocol XMLWritableAttr
  clojure.lang.Keyword
  (emit-attr [kwd val out]
    (emit-attr (name kwd) val out))

  String
  (emit-attr [name val ^Writer out]
    (doto out
      (.write name)
      (.write "='")
      (.write (xml-escape-attr (str val)))
      (.write "'")))

  clojure.lang.IPersistentVector
  (emit-attr [[kwd uri :as attr] val ^Writer out]
    (if uri
      (if-let [prefix (:prefix (meta attr))]
        (emit-attr (str prefix  \: (name kwd)) val out))
      (emit-attr kwd val out))))

(defn emit-element
  [elt ^Writer out]
  (let [tag (tag elt)
        attrs (attrs elt)
        content (content elt)
        prefix (prefix-for elt)
        tagname (str prefix (if prefix \:) (name tag))]
    (doto out
      (.write "<")
      (.write tagname))
    (doseq [[k v] attrs]
      (when-not (nil? v)
        (.write out " ")
        (emit-attr k v out)))
    (if (empty? content)
      (.write out "/>")
      (do
        (.write out ">")
        (doseq [child content]
          (emit-xml child out))
        (doto out
          (.write "</")
          (.write tagname)
          (.write ">"))))))

(defn emit-comment [^Comment c ^Writer out]
  (when (.contains (.text c) "--")
    (throw (IllegalArgumentException. "Illegal double-dash, \"--\", in comment.")))
  (doto out
    (.write "<!--")
    (.write (.text c))
    (.write "-->")))

(defn emit-cdata [^CData cd ^Writer out]
  (when (.contains (.text cd) "]]>")
    (throw (IllegalArgumentException. "Illegal end sequence, \"]]>\", in CDATA text.")))
  (doto out
    (.write "<![CDATA[")
    (.write (.text cd))
    (.write "]]>")))

(defn emit-pi [^PI pi ^Writer out]
  (let [{:keys [target text]} pi]
    ;; TODO - validate target: http://www.w3.org/TR/REC-xml/#sec-pi
    (when (.contains text "?>")
      (throw (IllegalArgumentException. "Illegal end sequence, \"?>\", in processing instruction.")))
    (doto out
      (.write "<?")
      (.write target)
      (.write " ")
      (.write text)
      (.write "?>"))))



(extend-protocol XMLWritable

  nil
  (emit-xml [_ _])
  
  Element
  (emit-xml [this out]
    (emit-element this out))

  Comment
  (emit-xml [this out]
    (emit-comment this out))

  CData
  (emit-xml [this out]
    (emit-cdata this out))

  PI
  (emit-xml [this out]
    (emit-pi this out))

  clojure.lang.Sequential
  (emit-xml [s ^Writer out]
    (doseq [o s]
      (emit-xml o out)))
  
  String
  (emit-xml [s ^Writer out]
    (.write out (xml-escape s)))

  ;; ???
  #_Object
  #_(emit-xml [o ^Writer out]
    (emit-xml (str o) out))
  
  )


(defn emit
  "Writes out the given object as XML.

  With one argument, returns the XML as a String.
  Otherwise, writes to the Writer supplied as the second argument.

  Note that the nodes argument can be of any type that implements
  the XmlWritable protocol, including a seq of other XmlWritable
  objects.
  "
  ([nodes]
     (let [to (java.io.StringWriter.)]
       (emit-xml nodes to)
       (str to)))
  ([nodes to]
     (binding [*emitter-state* (atom nil)]
       (emit-xml nodes to))))

