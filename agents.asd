;;;; Author Mark Royer

(defpackage #:agents-asd
  (:use :cl :asdf))

(in-package :agents-asd)

(defsystem agents
  :name "Agents"
  :author "Mark Royer"
  :version "0.1"
  :properties ((#:author-email . "mark.e.royer AT gmail.com")
		 (#:date . "Fall 2016")
		 ((#:albert #:output-dir) . "docs/")
		 ((#:albert #:formats) . ("docbook"))
		 ((#:albert #:docbook #:template) . "book")
		 ((#:albert #:docbook #:bgcolor) . "white")
		 ((#:albert #:docbook #:textcolor) . "black")
		 )
  :components ((:file "defpackage")
               (:file "simple-agents" :depends-on ("defpackage")))
  :depends-on ()
  )

