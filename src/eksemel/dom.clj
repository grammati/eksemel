(ns
  ^{:doc "Extend XML protocols to org.w3c.dom classes."}
  eksemel.dom
  (:require (eksemel [xml :as xml]))
  (:import (org.w3c.dom Document Element)))

(set! *warn-on-reflection* true)

(extend-protocol xml/ElementAccessor

  Element
  (tag [e]
    (keyword (.getTagName e)))
  (attrs [e]
    (xml/attrs->map (.getAttributes e)))
  (content [e]
    (.getChildNodes e))
  
  )

