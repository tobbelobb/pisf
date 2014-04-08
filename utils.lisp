(in-package :pisf)

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body body)
  `(aif ,test-form
        (progn ,@body)))

;; Destructive
(defun insert-after (lst index newelt)
  (push newelt (cdr (nthcdr index lst))) 
  lst)

;; Destructive
(defun delete-nth (lst index)
  (if (zerop index)
    (progn
      (lg :warn "delete-nth asked to delete 0'th element, which it can't do destructively.")
      (cdr lst))
    (let ((l (nthcdr (- index 1) lst)))
      (setf (cdr l) (cddr l))
      lst)))

(defun my-list->string (l)
  (if l (format nil "~{~a~^, ~}" l) "Empty"))

(defun my-string->list (string)
  (remove-if #'(lambda (s) (zerop (length s)))
	     (mapcar #'(lambda (s) (string-trim " " s)) 
		     (split-sequence #\, string))))

(defun my-string->word (string)
  (string-downcase (substitute #\- #\Space string)))

(defun strip-relpath-meta-count (relpath)
  (namestring 
    (make-pathname :name 
                   (strip-meta-count (pathname-name relpath)) 
                   :defaults relpath)))

(defun strip-meta-count (name)
  (subseq name 0 (scan '(:sequence #\- (:greedy-repetition 1 NIL :digit-class) :end-anchor) name)))

(defun paragraph->name (content)
  (name-conventions (first-three-words content)))

(defun relpath->db-name (relpath)
  (name-conventions (strip-meta-count (pathname-name relpath))))

(defun add-meta-to-filename (path metastr)
  (make-pathname :name (concatenate 'string (pathname-name path) metastr)
                 :defaults path))

(defun no-meta-counter? (path)
  (not (scan '(:sequence #\- (:greedy-repetition 1 NIL :digit-class) :end-anchor)
             (pathname-name path))))

(defun filename++ (path)
  (if (no-meta-counter? path)
    (add-meta-to-filename path "-0")
    (let* ((old-name (pathname-name path))
           (pos (1+ (search "-" old-name :from-end t)))
           (c (read-from-string (subseq old-name pos)))
           (new-name (format nil "~a~d" (subseq old-name 0 pos) (incf c))))
            (make-pathname :name new-name
                   :defaults path))))

(defun starts-with-source? (path)
  (let ((found? (search (namestring *source-pathname*) (namestring path) :test #'string=)))
    (and found? (= found? 0))))

(defun with-source (path)
  (if (starts-with-source? path)
    path
    (if (stringp path)
      (concatenate 'string (namestring *source-pathname*) path)
      (merge-pathnames path *source-pathname*))))

(defun unique-name (relpath)
  (do ((newpath (namestring relpath) (filename++ newpath)))
    ((not (probe-file (with-source (namestring newpath)))) newpath)))

(defun add-big (pic-path)
  (add-meta-to-filename pic-path "_big"))
  
(defun add-small (pic-path)
  (add-meta-to-filename pic-path "_small"))

(defun small-version (name)
  (add-small name))

(defun name->uri (name)
  (concatenate 'string "/" (url-encode (my-string->word name))))

(defun title->uri (title)
  (name->uri title))

(defun uri->path (uri)
  (if (> (length uri) 0)
    (concatenate 'string (namestring *source-pathname*) (string-left-trim "/" uri))
    (progn
      (lg :warn "Making empty uri into path")
      *source-pathname*))) 

(defgeneric given? (from-form)
  (:documentation "Checks if the data received from the form is non-empty"))

(defmethod given? ((from-form list))
  (not (null from-form)))

(defmethod given? ((from-form string))
  (not (zerop (length from-form))))

(defmethod given? ((from-form number))
  (not (< from-form 0)))

(defun relevant-leaves (checkbox-tree)
  (labels ((f (ct acc)
             (cond
               ((endp ct) acc)
               ((and (pair? (first ct))
                     (checked? (first ct))
                     (or (pair? (second ct)) (none-checked? (second ct))))
                (f (rest ct) (cons (name-extract (first ct)) acc)))
               ((not (pair? (first ct)))
                (f (rest (rest ct)) (nconc (f (first ct) nil) acc)))
               (t (f (rest ct) acc)))))
    (f checkbox-tree nil)))


(defun path-data->db-name (path-data)
  (destructuring-bind (#2=#:ignore file-name #1=#:ignore) 
    path-data
    (declare (ignore #1#))
    (declare (ignore #2#))
    (name-conventions (pathname-name file-name))))

