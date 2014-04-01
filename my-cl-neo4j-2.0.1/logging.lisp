(in-package :my-neo4j)

(defvar *log-level* 0)

(defparameter *log-levels* '(:info :debug :warn :error :none))

(defparameter *neo4j-log-stream* *standard-output*)

(defun set-log-level (level)
  (awhen (position level *log-levels*)
    (setq *log-level* it)
    level))

(defun get-log-level ()
  (nth *log-level* *log-levels*))

(defmacro lg (level str &rest args)
  (let ((n (position level *log-levels*)))
    (when n
      `(when (>= ,n *log-level*)
	        (format *neo4j-log-stream* ,(concatenate 'string "MY-NEO4j: " str) ,@args)))))

