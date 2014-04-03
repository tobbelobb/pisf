(in-package :pisf-backend)

(defparameter *source-pathname* #P"/home/torbjorn/Desktop/VanityRepRap/pisf/pisf-backend/")

;; Convenience variables and functions based on *source-pathname*
(defparameter *bush-merge-pathname* 
  (merge-pathnames #P"Merge_skeleton_bush.cyph" *source-pathname*))
(defparameter *constraints-pathname* 
  (merge-pathnames #P"Constraints_skeleton_bush.cyph" *source-pathname*))

; There's two init-situations one want to cover:
;  1: The graph-db is empty, and neo4j is not running
;  2: graph.db is populated, but neo4j is not running
; Will maybe see if I can cover these with MERGE later...

(defun init-constraints (&key (decode nil))
  (lg :info "Initiating constraints in database...")
  (with-open-file (in *constraints-pathname*
                      :direction :input)
    ;; in one bash, do cypher queries from the file, one line at a time.
    ;; One line in file should be one complete Cypher query
    (neo-request *batch* :decode decode
                 :method :post
                 :payload 
                 (do
                   ((i 0 (1+ i)) ;ids won't be 1, 2, 3 because of comments in file
                    (result '() 
                            (if (and (> (length line) 1) 
                                     (string= "//" 
                                              (subseq (string-left-trim " " line) 0 2)))
                              result ; Found comment line. Don't to anything.
                              (cons `(("method" . "POST")
                                      ("to" . ,*cypher*)
                                      ("id" . ,i)
                                      ("body" . (
                                                 ("query" . ,line)
                                                 ("params" . ())))) result)))
                    (line (read-line in nil 'eof)
                          (read-line in nil 'eof)))
                   ;; nreverse results so first line in file gets executed first
                   ((eq line 'eof) (nreverse result))))))

;; Extremely ineffective but ok for <~ 2k nodes...
;; For bigger than that we want to create via *bash* 
(defun merge-bush ()
  (cypher-query (file->string *bush-merge-pathname*)))

(defun brand-new-graph.db ()
  (init-constraints)
  (merge-bush))

