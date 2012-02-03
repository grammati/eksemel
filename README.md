# eksemel

XML wrangling for Clojure.

Eksemel can parse and emit XML similarly to clojure.xml, and in most
cases should be suitable as a drop-in replacement.

In addition, it supports:
 - namespace-aware parsing and emitting
 - XML comments, CData sections, and processing instructions
 - configurable handling of whitespace when parsing
 - keeping line/column numbers in metadata
 - utility functions for manipulating elements


## Usage

Leiningen:

```clojure
(defproject my-project ...
  :description ...
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [eksemel "0.1.0-SNAPSHOT"]])
```

Example:

```clojure
(ns my.ns
  (:require [eksemel.core :as xml]))

;; Parsing
(xml/parse "/path/to/some/file.xml")
(xml/parse "<some><xml in='a string'/><some>")

;; Emitting
(def e (xml/parse "<foo><bar/></foo>"))
(xml/emit e) ; returns a string
(xml/emit e *out*) ; writes to a Writer
```

## License

Copyright (C) 2011 Chris Perkins

Distributed under the Eclipse Public License, the same as Clojure.
