(in-package :pisf)

(setf (html-mode) :html5)
;;(defparameter *source-pathname* #P"/home/torbjorn/Desktop/VanityRepRap/pisf/")
(setf hunchentoot:*tmp-directory* (merge-pathnames "tmp/" *source-pathname*))
