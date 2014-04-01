;(defun label-node-by-id (id label)
;  ; can i create a node and label it in the same rest-call?
;  (http-request (format nil "~a/~a/labels" *node* id)
;                :method :post
;                :content-type "application/json"
;                :payload (let (( ))))) ; gives ca: "NIL can not name variable"

