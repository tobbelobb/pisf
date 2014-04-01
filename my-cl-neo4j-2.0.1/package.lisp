(defpackage #:my-cl-neo4j-2.0.1  
  (:nicknames :my-neo4j)
  (:use #:cl)
  (:import-from :drakma 
                :http-request)
  (:import-from :flexi-streams 
                :octets-to-string
                :flexi-stream-external-format
                :peek-byte)
  (:import-from :cl-json 
                :*json-output*
                :encode-json
                :encode-json-to-string
                :encode-json-alist-to-string
                :decode-json-from-string
                :json-bind
                :decode-json)
  (:import-from :split-sequence
                :split-sequence)
  (:documentation "A RESTful api to Neo4j that I know and understand myself.~
                   Not intended for usingn stand-alone. Just for building the ~
                   pisf-backend.")
  (:export ;; Functions from my-cl-neo4j-2.0.1. No by-id kind of functions
           #:bind-cypher-returns-lists
           #:bind-cypher-returns-singles
           #:bind-cypher-returns-and-loop
           #:neo-request
           #:transaction-request
           #:abort-transaction ; macro
           #:cypher-query
           #:get-node
           #:delete-node
           #:get-names-of-nodes
           #:get-rel-types
           #:get-property-keys
           ;#:get-indexes ; "legacy"... pff...
           ;; Utils
           #:lispify-output ; macro
           #:row-data-from-transaction-call
           #:ca
           ;; Managing database (from config.lisp)
           #:neo4j-executable
           #:get-constraints
           #:number-of-nodes-in-db
           #:number-of-rels-in-db
           #:database-empty?
           #:probe-database
           #:update-neo4j-uris
           ;; Neo4j-constants
           #:*neo4j-executable-path*
           #:*host*
           #:*data*
           #:*node*
           #:*node-index* ; What does node-index do?
           #:*rel-index*
           #:*rel-types*
           #:*batch*
           #:*cypher*
           #:*indexes*
           #:*constraints*
           #:*transaction*
           #:*labels*
           #:*neo4j-version*
           #:*rel*
;           ;; Logging constants
;           #:*log-level*
;           #:*neo4j-log-stream*
;           ;; Loggin functions/macros
;           #:set-log-level ; :info, :debug, :warn, :error or :none
;           #:get-log-level
;           #:lg ; like format, but only outputs when level >= *log-level*, and only into *neo4j-log-stream*
           ))
