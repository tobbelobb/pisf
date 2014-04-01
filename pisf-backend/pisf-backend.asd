(defsystem #:pisf-backend
  :name "The Print Issue Solution Filter's backend"
  :author "Torbjørn Ludvigsen <tobben@fastmail.fm>"
  :version "0.1"
  :description "Uses neo4j via my-cl-neo4j-2.0.1 REST API wrapper to serve ~
         the Print Issue Solution Filter the data it needs"
  :licence "GPLv3"
  :depends-on (#:my-cl-neo4j-2.0.1)
  :serial t
  :components
  ((:file "package")
   (:file "utils")
   (:file "logging")
   (:file "inits")
   (:file "pisf-backend")))
