(asdf:defsystem :pisf
  :name "Print Issue Solution Filter"
  :description "pisf: the system for building the print-issue-solution-filter web app"
  :version "0.1"
  :author "Torbjørn Ludvigsen <tobben@fastmail.fm>"
  :licence "GPL"
  :depends-on (#:hunchentoot #:parenscript #:cl-who #:cl-ppcre #:split-sequence #:string-case #:pisf-backend)
  :serial t
  :components ((:file "package")
               (:file "config")
               (:file "utils")
               (:file "logging")
               (:file "page-generating-macros")
               (:file "pisf")))
