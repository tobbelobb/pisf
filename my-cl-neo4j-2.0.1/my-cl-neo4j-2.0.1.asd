(defsystem #:my-cl-neo4j-2.0.1
  :name "My personal neo4j RESTful Client Interface"
  :author "Torbjørn Ludvigsen <tobben@fastmail.fm>"
  :version "0.1"
  :description "neo4j RESTful Client Interface"
  :licence "GPLv3"
  :depends-on (#:flexi-streams
               #:drakma
               #:cl-json
               #:split-sequence)
  :serial t
  :components
  ((:file "package")
   (:file "utils")
   (:file "config")
   (:file "logging")
   (:file "my-cl-neo4j-2.0.1")))

  ; Useful dependencies to check out later?
  ;:depends-on (:alexandria
  ;             :anaphora
  ;             :split-sequence
  ;             :babel
  ;             :cl-ppcre)
