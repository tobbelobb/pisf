(in-package :my-neo4j)
;; Conventions invended and followed in this source
;;  * relationship(s) is always shortened to rel(s)
;;  * Drakma calls the json content. Neo4j calls it payload. I call it payload
;;  * answers to create and set functions are not decoded by default
;;  * answers to get operations are decoded by default
;;  * answers to cypher statements are decoded by default
;;  * :decode defaults will all be nil after debugging
;;  * default :method :get is not written out
;;  * all timestamps should be set by Neo's timestamp() function to avoid
;;     confusion
;;  * Cypher should initially be used for everything excep retrieving metadata. 
;;     This way, we don't have to deal with id's directly


;; pisf-backend should use it and export functions using it, so :pisf can use
;; them

(defun got-cypher-data (cypher-ret-val)
  (ca :data cypher-ret-val))

(defmacro bind-cypher-returns-lists ((query &key params format-args) &body body) 
  (let ((cypher-ret-val (gensym "RET-VAL-"))
        (formatted-query (apply #'format nil query format-args)))
    `(let* ,(append
               `((,cypher-ret-val (cypher-query ,formatted-query
                                               :params ,params)))
               (loop for sym in (query->syms formatted-query)
                     and x = 0 then (1+ x)
                     collect `(,sym (mapcar #'(lambda (l) (elt l ,x)) 
                                            (ca :data ,cypher-ret-val)))))
     (when (got-cypher-data ,cypher-ret-val) ,@body))))

(defmacro bind-cypher-returns-singles ((query &key params format-args) &body body) 
  (let ((cypher-ret-val (gensym "RET-VAL-"))
        (formatted-query (apply #'format nil query format-args)))
    `(let* ,(append
               `((,cypher-ret-val (cypher-query ,formatted-query
                                               :params ,params)))
               (loop for sym in (query->syms formatted-query)
                     and x = 0 then (1+ x)
                     collect `(,sym (car (mapcar #'(lambda (l) (elt l ,x)) 
                                            (ca :data ,cypher-ret-val))))))
     (when (got-cypher-data ,cypher-ret-val) ,@body))))

(defmacro bind-cypher-returns-and-loop ((query &key params format-args) &body body) 
  (let* ((formatted-query (apply #'format nil query format-args))
         (syms (query->syms formatted-query)))
    `(let ((ret-lists (ca :data (cypher-query ,formatted-query :params ,params))))
       (when ret-lists
         (do* ,(append 
                 `((ret-list (pop ret-lists) (pop ret-lists))) 
                 (loop for sym in syms and x = 0 then (1+ x)
                       collect `(,sym (pop ret-list) (pop ret-list))))
           ((null ,(first syms)))
           ,@body)))))

;;(macroexpand-1 '(bind-cypher-returns-and-loop ("MATCH (ma:Mini_article) RETURN ma.name, ma.title") 
;;                  (format t "~a~%" ma.name)
;;                  (format t "~a~%" ma.title)))
;;
;; 

;; Handling neo4j's REST API starts here

; Raw http requests to neo4j server.
; Directly from REST API manual
; adds content-type, accept, and X-Stream : true header to all calls
; Not 100% necessary, but easiest on the server, according to REST API manual
; Dont know if :want-stream and X-stream is worth it or even working.
(defun neo-request (uri &optional &key (decode nil) (method :get) (payload nil))
  (lg :info "Sending request to ~a~%" uri)
  (multiple-value-bind (in-stream return-code #2=#:ignore #3=#:ignore #4=#:ignore must-close
                                  reason-string)
    (http-request uri :method method
                  :content-type "application/json"
                  :accept "application/json"
                  :external-format-out :utf-8
                  :external-format-in :utf-8
                  :content (encode-json-to-string payload) ; Don't know if Drakma sends "JSON stream"?
                  :want-stream t      ;X-Stream tells Neo if we're sending a "JSON stream"....
                  :additional-headers `(("X-stream" . ,(if payload "true" "false"))))
    (declare (ignore #2#))
    (declare (ignore #3#))
    (declare (ignore #4#))
    (setf (flexi-stream-external-format in-stream) :utf-8)
    (let ((ret-val (when (and decode 
                              (not (eq 'eof (peek-byte in-stream nil nil 'eof))))
                     (decode-json in-stream))))
      (when must-close (close in-stream))
      (values ret-val return-code reason-string))))

(defun transaction-request (statement parameters &key (commit-uri nil) (commit? nil))
  (let ((content (statements-json-list 
                 `(("statement" . ,statement)
                   ("parameters" . ,parameters)))))
  (lispify-output
    (http-request
      (cond ; Deciding which uri to send to
        ((and commit-uri commit?) 
          (progn (lg :debug "Commiting transaction ~a.~%~
                             Content: ~a~%" commit-uri content) commit-uri))
        (commit-uri 
          (progn (lg :debug "Adding to transaction ~a~%~
                             Content: ~a~%" (string-right-trim "/commit" commit-uri) content) 
                 (string-right-trim "/commit" commit-uri)))
        (commit? 
          (progn (lg :debug "Creating and commiting transaction~%~
                             Content: ~a~%" content)
                 (format nil "~a/~a" *transaction* "commit"))) ; Commit transaction directly...
        (t (progn (lg :debug "Initiating transaction.~%~
                              Content: ~a~%" content)
                  *transaction*)))
      :method :post
      :content-type "application/json"
      :content content))))

;; This is a macro just because of ,@format-args
;; How to do that with a function?..
(defmacro abort-transaction (commit-uri message &rest format-args)
  `(progn (http-request (string-right-trim "commit" ,commit-uri) :method :delete) ; deletes the transaction
          (lg :warn "Aborting transaction ~a~%" ,commit-uri)
          (lg :error ,message ,@format-args)))

;; payload is an alist ("looks" . "funny")
;; Returns (values body status) from HTTP-REQUEST
(defun create-node (&optional &key (payload nil) (decode nil))
  (neo-request *node* :method :post
                      :payload payload
                      :decode decode))

;; To create multiple nodes with props
;; (cypher-query "CREATE (n:Person {props}) RETURN n"
;;               '((props . 
;;                 #((("name" . "Andres") ("position" . "Developer"))
;;                   (("name" . "Peter") ("position"  . "Developer"))))))
(defun cypher-query (query &key (params ()) (decode t))
  (lg :info "Cypher query: ~a~%sent with params ~a~%" query params)
  (multiple-value-bind (ret-val ret-code reason)
    (neo-request *cypher* 
                 :method :post
                 :payload `(("query" . ,query)
                            ("params" . ,params))
                 :decode decode)
    (when (not (ca :data ret-val))
      (lg :warn "Cypher-query didn't return any data~%"))
    (values ret-val ret-code reason)))
    

(defun simple-query (label name do-with-node)
  (cypher-query 
    (format nil "MATCH (a:~a {name:{name}}) ~a a" label do-with-node)
    `(("name" . ,name))))

(defun get-node (label name)
  (simple-query label name "RETURN"))

(defun delete-node (label name)
  (simple-query label name "DELETE"))

(defun by-id (id &key (method :get) (node-or-rel *node*) (decode nil))
  (neo-request (format nil "~a/~a" node-or-rel id)
                :decode decode
                :method method))

; users of by-id mainly for resemblence with normal English language
(defun get-node-by-id (id &key (decode t))
  (by-id id 
         :node-or-rel *node* 
         :decode decode))

(defun delete-node-by-id (id)
  (by-id id 
         :method :delete 
         :node-or-rel *node*))

;; The /db/data/relationship request uri (*rel*), not given by
;; (get-service-root) is used here
(defun get-rel-by-id (id &key (decode t))
  (by-id id 
         :node-or-rel *rel*
         :decode decode))

;; Simply returned nil and did nothing, when asked to delete a relationhip
;; that was one of four exactly identical ones. (id 49 from Slic3r to Slicer)
;; Cypher matched all the four rels and deleted them. More reliable.
;; Must check how duplicate rels got there in first place
(defun delete-rel-by-id (id)
  (by-id id 
         :method :delete 
         :node-or-rel *rel*))

(defun get-properties-for-node-by-id (id &key (decode t))
  (neo-request (format nil "~a/~a/properties" *node* id)
               :decode decode))

(defun get-rels-of-node-by-id (id &key (direction "all") (rel-type "") (decode t))
  ;; :direction can be "in", "out" or "all".
  ;; :rel-type can be several rel-types separated with &
  ;; ampersand may be necessary to encode like %26
    (neo-request (format nil "~a/~a/relationships/~a/~a" *node* id direction rel-type)
                 :decode decode))

;; Returns all known relation types, not only the ones in current use
(defun get-rel-types (&key (decode t))
  (neo-request *rel-types*
               :decode decode))

(defun create-rel-by-ids (from-id to-id rel-type &key (payload nil) (decode nil))
  (neo-request (format nil "~a/~a/relationships" *node* from-id)
                :method :post
                :payload 
                `(("to" .  ,(format nil "~a/~a" *node* to-id)) 
                  ("type" . ,rel-type)
                  ("data" . ,payload))
                :decode decode))

;; Setting property to t becomes true in database
(defun set-property-by-id (id key value)
  (neo-request (format nil "~a/~a/properties/~a" *node* id key)
                :method :put
                :payload value))

(defun delete-all-properties-by-id (id) ;doesn't return json
  (neo-request (format nil "~a/~a/properties" *node* id)
                :method :delete))

(defun get-constraints (&key (decode t))
  (neo-request *constraints*  
               :decode decode))
                              
(defun get-id-of-nodes (&key (limit 10) (decode t))
  (mapcar #'car
          (ca :data (cypher-query "MATCH a RETURN id(a) LIMIT {lim}" 
                                          :params `(("lim" . ,limit))
                                          :decode decode))))

(defun get-names-of-nodes (&key (limit 10) (decode t) (label nil))
  (mapcar #'car ;this is awkward, but depends on the way Neo returns data in rows and columns
          (ca :data (cypher-query 
                      (if label 
                        (format nil "MATCH (a:~a) RETURN a.name LIMIT {lim}" label)
                        "MATCH (a) RETURN a.name LIMIT {lim}")
                      :params `(("lim" . ,limit))
                      :decode decode))))

;; Does this not work because it is "legacy indexing"?
(defun get-indexes () ; will not return uniqueness constraints
  (neo-request *node-index* :decode t))

(defun get-property-keys ()
  (neo-request (format nil "~a/~a" *data* "propertykeys") :decode t))

(defun number-of-nodes-in-db () ; This should be known somwhere internal in neo4j...
  (car (car (ca :data (cypher-query "MATCH a RETURN count(a)")))))

(defun number-of-rels-in-db ()
  (car (car (ca :data (cypher-query "MATCH a-[r]->b RETURN count(r)")))))

(defun database-empty? ()
  (not (get-id-of-nodes :limit 1 :decode t)))

(defun probe-database ()
  (let ((response-code (nth-value 1 (http-request *host*))))
    (case response-code
      (200       (format t "Neo4j running and responding"))
      (otherwise (format t "PROBE-DATABASE got response code: ~a ~
                             from host: ~a" response-code *host*)))))

;; Should be able to do this in one transaction with a transaction-request...
;; Rewrite later
(defun delete-node-and-its-relationships-by-id (id &key (decode nil))
  (let ((rel-uris (loop for rel in (get-rels-of-node-by-id id)
                        collect (cdr (assoc :self rel)))))
    (neo-request *batch* 
                 :method :post
                 :decode decode
                 :payload 
                 (do*
                   ((i (length rel-uris) (1- i))
                    (result `((("method" . "DELETE")
                               ("to" . ,(format nil "~a/~a" *node* id))
                               ("id" . ,(length rel-uris))))
                            (cons `(("method" . "DELETE")
                                    ("to" . ,(car rel-uris))
                                    ("id" . ,i)) result))
                    ; creating inner local copy of rel-uris
                    (rel-uris rel-uris (cdr rel-uris)))
                   ((null rel-uris) result)))))
