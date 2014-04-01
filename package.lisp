(defpackage :print-issue-solution-filter
  (:nicknames :pisf)
  (:use :cl :pisf-backend)
  (:import-from :hunchentoot
                :create-static-file-dispatcher-and-handler
                :*dispatch-table*
                :start
                :easy-acceptor
                :create-prefix-dispatcher
                :define-easy-handler
                :url-encode
                :url-decode
                :request-uri
                :referer
                :*request*
                :get-parameters
                :redirect)
  (:import-from :parenscript
                :ps
                :chain
                :@)
  (:import-from :cl-who
                :with-html-output-to-string
                :with-html-output
                :html-mode
                :escape-string
                :esc
                :htm
                :str
                :fmt)
  (:import-from :cl-ppcre
                :regex-replace
                :scan
                :all-matches
                :all-matches-as-strings)
  (:import-from :split-sequence
                :split-sequence)
  (:import-from :my-cl-neo4j-2.0.1
                :cypher-query
                :bind-cypher-returns-lists; Just during development
                :bind-cypher-returns-and-loop
                :bind-cypher-returns-singles); Just during development
  (:import-from :string-case
                :string-case)
  (:documentation "A web app for building and sorting mini-articles on the web.~ 
                  Ment as an online tool for the RepRap online community to help~ 
                  store, sort and search among it's vast collective~ 
                  troubleshooting knowledge."))
