(in-package :my-neo4j)

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body body)
  `(aif ,test-form
        (progn ,@body)))

;; Parses octet-arrays from neo4j.
;; A bit slow?
(defmacro lispify-output (form)
  (let ((resp (gensym "CONTENTS-"))
        (resp-code (gensym "CODE-")))
    `(multiple-value-bind (,resp ,resp-code) ,form
      (values (my-decode-json-string (octets-to-string ,resp))
              ,resp-code))))

;; used only as helper to bind-cypher-returns macro
(defun query->syms (query)
  (mapcar #'(lambda (str) (intern (string-upcase (string-trim " " str)))) 
          (split-sequence:split-sequence 
            #\, 
            (subseq query 
                    (+ 7 ; 7 letters in " RETURN"
                       (search "RETURN" query :from-end t))) 
            :test #'char=)))

; Why doesn't decode-json-from string handle empty strings by default?
; to do : check it up
(defun my-decode-json-string (s) ; Used by lispify-output
  (unless (string= s "")
    (decode-json-from-string s)))

(defun ca (item alist)
  (cdr (assoc item alist)))

(defun row-data-from-transaction-call (from-neo)
  (mapcar #'cdar (ca :data (car (ca :results from-neo)))))

;; Should have been used instead:
;(defmacro with-neo4j-response-vars (varlist &body body)
;  `(json-bind ,varlist 

;; Supposed to collect values from all key-value-pairs where
;; key matches var. I would rather have this than lispify-output+assoc
;; (defmacro json-bind-multiple ((&rest vars) json-source &body body)

;; Should roll my own json-encoder...

;; Used in transaction request
(defun statements-json-list (alist) 
  (format nil "{\"statements\" : [~a]}"
          (encode-json-to-string alist)))

