(in-package :my-neo4j)

(defparameter *host* "http://localhost:7474/")
;; For https requests:
;; (defparameter *host* "http://localhost:7473/")


;; Convenience functions and variables based on *host* value
(defun add-host (request-uri)
  (concatenate 'string *host* request-uri))

(defparameter *data* (add-host "db/data"))
(defun add-data (s)
  (concatenate 'string *data* s))
;; The following uris can be updated through Neo4j's REST API with
;; (get-service-root)
(defparameter *node* (add-data "/node"))
(defparameter *node-index* (add-data "/index/node"))
(defparameter *rel-index* (add-data "/index/relationship"))
(defparameter *rel-types* (add-data "/relationship/types"))
(defparameter *batch* (add-data "/batch"))
(defparameter *cypher* (add-data "/cypher"))
(defparameter *indexes* (add-data "/schema/indexes"))
(defparameter *constraints* (add-data "/schema/constraint"))
(defparameter *transaction* (add-data "/transaction"))
(defparameter *labels* (add-data "/labels"))
(defparameter *neo4j-version* "Unknown")
;; Interface doesn't give any relationship request-uri... Why?
(defparameter *rel* (add-data "/relationship"))

;; When POST'ed to its db/data request uri, neo4j responds with all its other
;; request uris. We use the ones Neo4j provides itself rather than hard coding
;; what's in the current REST API manual.
(defun get-service-root ()
  (lispify-output (http-request *data*))) ; Can't run if neo4j isn't running...

;; Sould definitely run this from some sort of init-pisf-backend
(defun update-neo4j-uris ()
  (let ((service-root-answer (get-service-root)))
    (setf *node* (ca :node service-root-answer))
    (setf *node-index* (ca :node--index service-root-answer))
    (setf *rel-index* (ca :relationship--index service-root-answer))
    (setf *rel-types* (ca :relationship--types service-root-answer))
    (setf *batch* (ca :batch service-root-answer))
    (setf *cypher* (ca :cypher service-root-answer))
    (setf *indexes* (ca :indexes service-root-answer))
    (setf *constraints* (ca :constraints service-root-answer))
    (setf *transaction* (ca :transaction service-root-answer))
    (setf *labels* (ca :node--labels service-root-answer))
    (setf *neo4j-version* (ca :neo-4-j--version service-root-answer))))


