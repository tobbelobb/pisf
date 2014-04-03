(defpackage :print-issue-solution-filter-backend
  (:nicknames :pisf-backend) 
  (:use :cl :my-cl-neo4j-2.0.1)
  (:documentation "A tool for building and accessing a structured knowledge database ~
                   for the RepRap community.")
  (:export ; functions
           #:insert-mini-article 
           #:vote-for
           #:ma-titles
           #:ma-exists?
           #:name-and-label-exist?
           #:children-of
           #:categories-query 
           #:insert-all-mini-articles 
           #:update-property-for-all-mas
           #:insert-problem 
           #:insert-solution 
           #:insert-picture 
           #:insert-figure 
           #:insert-paragraph 
           #:create-bush-node 
           #:delete-category-node 
           #:init-constraints
           #:merge-bush
           #:brand-new-graph.db 
           #:name-conventions 
           #:transactional-names-exist-with-label? 
           ; macros
           #:name-conventionize 
           #:if-elements-with-label-exist-bind-commit-uri
           ; ymse
           #:ca
           #:first-three-words
           #:listify
           ))
