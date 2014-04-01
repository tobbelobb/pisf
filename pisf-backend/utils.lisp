(in-package :pisf-backend)

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body body)
  `(aif ,test-form
        (progn ,@body)))

(defun simple-extract (neo4j-alist)
  (mapcar #'car (ca :data neo4j-alist)))

(defun extract-commit-uri (resp)
  (cdr (assoc :commit resp)))

(defun listify (s)
  (if (not (listp s)) (list s) s))

(defun all-assoc (string alist)
  (loop for conspair in alist 
        when (string= string (car conspair)) 
        collect (cdr conspair)))

(defun first-three-words (p)
  (let ((p (string-left-trim " " p)))
    (if (string= "" p) ""
      (do* 
        ((x 0 (position #\Space p :test #'char= :start (+ 1 x)))
         (i 0 (1+ i)))
        ((or (null x) (= 3 i)) (string-right-trim " " (subseq p 0 (when x x))))))))

; Works on dotted pairs
(defun flatten (obj)
  (do* ((result (list obj))
        (node result))
       ((null node) (delete nil result))
    (cond ((consp (car node))
           (when (cdar node) (push (cdar node) (cdr node)))
           (setf (car node) (caar node)))
          (t (setf node (cdr node))))))

(defun file->string (filename)
  (with-open-file (in filename
                      :direction :input)
    (do*
      ((result "" 
               (if (and (> (length line) 1) 
                        (string= "//" 
                                 (subseq (string-left-trim " " line) 0 2)))
                 result ; Found comment line. Don't to anything.
                 (concatenate 'string 
                               result " " line)))
       (line (read-line in nil 'eof)
             (read-line in nil 'eof)))
      ((eq line 'eof) result))))

(defun name-element-in-common? (list1 list2)
  (< (length (set-difference list1 list2 :test #'string=))
             (length list1)))

(defun duplicate-name-element? (list1 list2)
  (< (length (remove-duplicates (append list1 list2) :test #'string=))
             (length (append list1 list2))))

