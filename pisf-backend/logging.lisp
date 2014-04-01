(in-package :pisf-backend)

(defvar *log-level* 0)

(defparameter *log-levels* '(:info :debug :warn :error :none))

(defparameter *pisf-backend-log-stream* *standard-output*)

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
	        (format *pisf-backend-log-stream* ,(concatenate 'string "PISF-BACKEND: " str) ,@args)))))

