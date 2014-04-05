(in-package :pisf-backend)

(defmacro dodge-empty-names (names &body body)
  `(if (loop for name in (flatten (list ,@names))
             always (and (stringp name) (> (length (string-left-trim " " name)) 0)))
     (progn
       ,@body)
     (lg :warn "Got a zero length, empty or non-string name.")))

(defun dodge-tester (name1 &optional (name-list nil) (name-list2 nil))
  (dodge-empty-names (name1 name-list name-list2)
    (format nil "none were empty")))

(defun name-conventions (n)
  (format nil "~:(~a~)" (string-trim " " n))) ; What happens with newlines and tabs?

;; The idea is:
;; Name variables should be rebound to values that are ensured to follow
;; the following conventions:
;;  - Names can come as strings or lists of strings
;;  - Strings can not have length 0 or consist of only whitespace
;;  - Lists can have length 0 (then they contain no names)
;;  - First letter after space in any name is capital, all other are lower case
;;  - Name of the function that received zero-length name should be logged...
;;     Name of function is not logged as for now
;; 
(defmacro name-conventionize (names &body body)
  `(dodge-empty-names ,names
     (let ,(loop for name in names
                  collect
                  `(,name (cond 
                            ((stringp ,name) (name-conventions ,name)) ; Its a name
                            ((and (consp (car ,name)) (stringp (cdar ,name))) ; Its an alist of names
                             (mapcar 
                               #'(lambda (x) (cons (name-conventions (car x)) (name-conventions (cdr x))))
                               ,name))
                            ((listp ,name) (mapcar #'name-conventions ,name))
                            (t (lg :error "Tried to name-conventionize something unknown: ~a~%" ,name))))) 
       ,@body)))

(defun conv-test (name1 name-list name-list2)
 (name-conventionize (name1 name-list name-list2)
    (format nil "name1: ~s~%~
             name-list: (~{~s~^, ~})~%~
             name-list2: (~{~s~^, ~})" name1 name-list name-list2)))

;; Should return 3 values: 
;; Value0: Names of the non-existent categories,
;; Value1: ids of the existent ones
;; Value2: commit-uri for transaction that was opened 
;; Calling function can precede with transactions to commit-point, knowing
;; the names still exist and ids are still valid.
(defun transactional-names-exist-with-label? (names label &key (commit-uri nil) (commit? nil))
  (let* ((names (if (stringp names) (list names) names))
         (all-data (transaction-request
                     (format nil "MATCH (a:~a) WHERE a.name in {names} ~
                                  RETURN a.name, id(a)" label)
                     `(("names" . ,(coerce names 'simple-vector)))
                     :commit-uri commit-uri
                     :commit? commit?))
         (rows (row-data-from-transaction-call all-data))
         (found-names (mapcar #'car rows))
         (ids (mapcar #'cadr rows))
         (not-found (set-difference names found-names :test #'string=)))
    (values not-found ids (ca :commit all-data))))

;; commit? is not in lambda list because you should never commit a pure check...
;; If this was a function, and not a macro, we would return commit-uri or nil.
;; Then we would have to write out if ... ourselves...

(defmacro if-elements-with-label-exist-bind-commit-uri (names label commit-uri &body body) 
    `(multiple-value-bind (not-found-ones #1=#:ignore commit-uri)
      (if (null ,names) (values nil nil ,commit-uri)
        (transactional-names-exist-with-label? ,names ,label :commit-uri ,commit-uri))
      (declare (ignore #1#))
      (if not-found-ones 
        (progn
          (abort-transaction commit-uri 
                             "~a ~{~#[~;~a~;~a and ~a~:;~@{~a~#[~; and ~:;, ~]~}~]~} not found in graph.~%" 
                             ,label not-found-ones)
          (values (list not-found-ones nil ,label) 999)) ;; If bush node is missing we want '("name" nil "Bush")
        (progn ,@body))))

;; The idea is this:
;;  - Check if all super categories exist
;;  - If they do, then we will access them via the returned uris
;;    -- How to we do anything other than Cypher via transactional endpoint?
;;    This function is case insensitive because of name-conventionize
;;    Should also include sub-categories. Only a matter of changing cyper
;;    queries slightly
;; To extract ignored ids from first query:
;; (car (ca :row (car (ca :data (car (ca :results all-of-it))))))
;; subs are list of names of either :Category or :Part nodes
(defun create-bush-node (name super-categories &key (subs nil) (cat-or-part "Category")) 
  (if 
    (and (consp super-categories) ; All bush nodes must have super-categories. This could be tolt to compiler, rather than tested at runtime
         (stringp (first super-categories)) ; This could be tolt to compiler, rather than tested at runtime
         (or (string= "Category" cat-or-part) (string= "Part" cat-or-part))
         (not (and (string= "Part" cat-or-part) subs))) ; Parts can not have sub-categories
    (name-conventionize 
      (name super-categories subs)
      (if 
        (duplicate-name-element? super-categories subs)     
        (lg :error "Tried to create category node with duplicate sub/super-name: ~%~
                    super-categories: ~a~%~
                    subs: ~a~%" super-categories subs)
        (if-elements-with-label-exist-bind-commit-uri 
          super-categories ; Names of categories
          "Category" ; The label I want them under
          nil ; The commit-uri to continue on. Nil initiates new transaction
          (if-elements-with-label-exist-bind-commit-uri ;; Accepts empty lists silently
            subs 
            "Bush" 
            commit-uri 
            (transaction-request
              (format nil ; Decide statement. MERGE in FOREACH should be changed for a MATCH somehow
                      "CREATE ~
                       (new:~a:Bush {name : {name}, created:timestamp(), accessed:timestamp()}) ~
                       WITH new ~
                       FOREACH (super_name in {super_names} | ~
                                           MERGE (super:Category {name:super_name}) ~
                                           CREATE (new)-[:IS]->(super) ~
                                           SET super.accessed = timestamp()) ~
                       ~@[FOREACH (sub_name in {sub_names} | ~
                                            MERGE (sub:Bush {name:sub_name}) ~
                                            CREATE (sub)-[:IS]->(new) ~
                                            SET sub.accessed = timestamp())~] ~
                       RETURN new.name" cat-or-part subs) 
                  (append `(("name" . ,name)
                            ("super_names" .  ,(coerce super-categories 'simple-vector)))
                            (when subs `(("sub_names" . ,(coerce subs 'simple-vector)))))
              :commit-uri commit-uri :commit? t))))) ; completes the transaction
      (lg :error "Cannot create Category/Part ~a~%" name))) 

;; Part node must have custom way to put specs. Category node should maybe have this as well?
(defun set-part-specs (name props)
  (cypher-query "MATCH (p:Part name:{name}) SET p.{props} RETURN p.{props}"
                :params `(("name" . ,name) ("props" . ,props))))

;; Find sub-categories and super-categories, bind them together in one cypher
;; This let* all-of-it pattern repeats itself. 
;; Maybe worth writing a macro for.
;; Sometimes, the second cypher statment is conditional
;; Sometimes, we want to extract ceirtain data from all-of-it in a special
;; way... So we need a lambda sent in to the macro for each of those...
(defun delete-category-node (name) ; Move childrens IS-relations to its parents before dying
  (let* ((all-of-it  
           (transaction-request
             (format nil
                     "MATCH (a:Category {name : {name}}), ~
                      (a) -[delete1:IS]-> (super:Category), ~
                      (sub:Bush) -[delete2:IS]-> (a) ~
                      WITH a, sub, super, delete1, delete2 ~
                      MERGE (sub) -[r:IS]-> (super) ~
                      ON CREATE SET sub.accessed=timestamp(), ~
                      super.accessed=timestamp() ~
                      DELETE delete1, delete2 ~
                      RETURN id(a)") `(("name" . ,name)))) ; Opens new transaction
        (commit-uri (ca :commit all-of-it)))
    (when (not (ca :data (car (ca :results all-of-it))))
      (lg :warn "Tried to delete category: ~a~%~
                 Category with this name was not found in db, or was an orphan.~%" name))
    (transaction-request
      (format nil "MATCH (a:Category {name:{name}})  DELETE a")
      `(("name" . ,name))
      :commit-uri commit-uri :commit? t))) ; Commits the transaction

;; Try to use this one if repeated patterns seems to work
;(defmacro assure-names-in-categories (names-lists label-list &body body)
;  `(name-conventionize ,names-lists
;     (bind-not-found-ones-and-commit-uri ,(first names-lists) ,(first label-list) nil ; Opens new transaction
;        ,(loop for names-list in (rest names-lists) and label in label-list
;               (setf result (cons `(bind-not-found-ones-and-commit-uri ,names-list ,label commit-uri))))


;; Learn how to load this
;; Do new queries to all graph-altering functions
(defun insert-paragraph (content is-about ; IS-ABOUT relations point into :Bush
                           &key 
                           (name nil); Generate name if not given
                           (explained-by nil) 
                           (explains nil) 
                           (commit-uri nil)
                           (commit? t)) 
  (if (and (stringp content) (not (string= "" content)) 
           (or is-about 
               (and (null explains) (null explained-by))))
    (let ((name (if name name (first-three-words content)))) ; Construct name
      (name-conventionize 
        (name is-about explained-by explains)
        (if-elements-with-label-exist-bind-commit-uri ; would accept an empty list
          is-about
          "Bush" 
          commit-uri ; May initiate new transaction
          (if-elements-with-label-exist-bind-commit-uri
            (append explained-by explains)
            "Picture"
            commit-uri
            (transaction-request
              (format nil ; MERGE is used inside FOREACH beacause MATCH is'nt allowed. Should use MATCH somehow
                      "~:[~;MATCH (b:Bush) ~] ~
                      ~:[~;, (explained_by:Picture) ~] ~
                      ~:[~;, (explains:Picture) ~] ~
                      ~:[~; WHERE b.name            IN {bush_names} ~] ~
                    ~:[~;AND explained_by.name IN {explained_by}~] ~
                    ~:[~;AND explains.name     IN {explains}~] ~
                    ~:[~; SET b.accessed            = timestamp() ~]~
                    ~:[~;, explained_by.accessed = timestamp()~] ~
                    ~:[~;, explains.accessed     = timestamp()~] ~
                       MERGE (new:Paragraph {name : {name}, content : {content}}) ~
                         ON CREATE SET new.created=timestamp() ~
                  ~:[~;MERGE (new) -[:IS_ABOUT]->(b) ~]~
                  ~:[~;MERGE (new)<-[:EXPLAINS]- (explained_by)~] ~
                  ~:[~;MERGE (new) -[:EXPLAINS]->(explains)~] ~
                       RETURN new.name"
                 is-about explained-by explains is-about explained-by explains is-about
                 explained-by explains is-about explained-by explains) 
              (append `(("name" . ,name)
                        ("content" . ,content)
                        ("bush_names" . ,(coerce (listify is-about) 'simple-vector)))
                      (loop for l in (list explained-by explains) 
                            and param_name in '("explained_by" "explains") 
                            when l 
                            collect `(,param_name . ,(coerce (listify l) 'simple-vector))))
              :commit-uri commit-uri :commit? commit?)))))
    (lg :warn "Tried to create paragraph or without IS-ABOUT-relations")));; Should create paragraph node and :IS_ABOUT relations


;; Assure the depicts stuff is in :Bush. No need to name-conventionize
;; "depicts"
;; However, "name" must be name-conventionized, as it is inserted
;; Inserted paths should definitely be absolute, so pisf can use them
;; directly...

;; Picture may be inserted without any relations
;; What to do if picture doesn't get tagged? redirect to special tagging page!
(defun insert-picture (path depicts ; DEPICTS rels point into :Bush.
                            &key
                            (name nil) ; Generate name from filename if not given
                            (explains nil)
                            (explained-by nil)
                            (height nil) (width nil)
                            (commit? t)
                            (commit-uri nil)) 
  (let ((name (if name 
                (name-conventions name) 
                (name-conventions (pathname-name path))))) ; Generate filename
    (if-elements-with-label-exist-bind-commit-uri
      depicts ; accepts empty list silently
      "Bush"
      commit-uri ; Initiate transaction
      (if-elements-with-label-exist-bind-commit-uri
        (append explained-by explains)
        "Picture"
        commit-uri
        (transaction-request
          (format nil
                  "MATCH (b:Bush) ~
                  ~:[~;, (explained_by:Picture) ~] ~
                  ~:[~;, (explains:Picture) ~] ~
                   WHERE b.name            IN {bush_names} ~
                ~:[~;AND explained_by.name IN {explained_by}~] ~
                ~:[~;AND explains.name     IN {explains}~] ~
                   SET b.accessed            = timestamp() ~
                ~:[~;, explained_by.accessed = timestamp()~] ~
                ~:[~;, explains.accessed     = timestamp()~] ~
                   MERGE (new:Picture {name : {name}, ~
                                       path : {path}~
                               ~:[~;, width : {width}~]~
                              ~:[~;, height : {height}~]}) ~
                     ON CREATE SET new.created=timestamp() ~
                   MERGE (new) -[:DEPICTS ]->(b) ~
              ~:[~;MERGE (new)<-[:EXPLAINS]- (explained_by)~] ~
              ~:[~;MERGE (new) -[:EXPLAINS]->(explains)~] ~
                   RETURN new.name"
                   explained-by explains explained-by explains 
                   explained-by explains width height explained-by explains)
          (append `(("name" . ,name)
                    ("path" . ,(namestring path))
                    ("bush_names" . ,(coerce (listify depicts) 'simple-vector)))
                  (loop for l1 in (list width height)          and l2 in (list explains explained-by)
                        and param-name1 in '("width" "height") and param-name2 in '("explains" "explained_by")
                        when l1 collect `(,param-name1 . ,l1)
                        when l2 collect `(,param-name2 . ,(coerce (listify l2) 'simple-vector)))) 
          :commit-uri commit-uri :commit? commit?)))))


;; Insert the mini-article into the database
;; If everything is well, we want nil

;; If another node exist, we want for example '("name" t "Picture)
(defun insert-mini-article (title ; gives Mini_article name and title
                            problem-picture-path ; gives picture path and maybe picture name.
                            solution-picture-path ; all paths in database are relative to source-path
                            &key
                            problem-description ; gives description paragraph content and maybe problem paragraph name
                            solution-description 
                            problem-description-bush-names
                            solution-description-bush-names
                            problem-name
                            solution-name
                            prob-pic-bush-names
                            sol-pic-bush-names
                            prob-pic-caption
                            sol-pic-caption
                            prob-pic-caption-name
                            sol-pic-caption-name)
  (if (and (or problem-name (and problem-description problem-description-bush-names)) ; Must be able to find of define problem and solution
           (or solution-name (and solution-description solution-description-bush-names)))
    (name-conventionize (solution-description-bush-names 
                         problem-description-bush-names
                         prob-pic-bush-names
                         sol-pic-bush-names)
      (let* ((name (name-conventions title))
             (problem-name  (aif problem-name  (name-conventions it) (name-conventions (first-three-words problem-description))))
             (solution-name (aif solution-name (name-conventions it) (name-conventions (first-three-words solution-description))))
             (prob-pic-name (name-conventions (pathname-name problem-picture-path)))
             (sol-pic-name  (name-conventions (pathname-name solution-picture-path)))
             (problem-picture-path  (namestring problem-picture-path))
             (solution-picture-path (namestring solution-picture-path))
             (prob-pic-caption-name  (aif prob-pic-caption-name  (name-conventions it) 
                                       (when prob-pic-caption (name-conventions (first-three-words prob-pic-caption)))))
             (sol-pic-caption-name   (aif sol-pic-caption-name   (name-conventions it) 
                                       (when sol-pic-caption  (name-conventions (first-three-words sol-pic-caption))))))
        (if-elements-with-label-exist-bind-commit-uri  ;; If bush-node is missing we want '("name" nil "Bush")
          (remove-duplicates (flatten (list solution-description-bush-names 
                                            problem-description-bush-names
                                            prob-pic-bush-names
                                            sol-pic-bush-names)) :test #'string=)
          "Bush"
          nil ; initiates transaction and creates commit-uri   
            (transaction-request
              (format nil
                    "MATCH ~
                     (prob_bush:Bush), ~
                     (sol_bush:Bush) ~
              ~:[~;, (prob_pic_bush:Bush)~] ~
              ~:[~;, (sol_pic_bush:Bush) ~] ~
                     WHERE prob_bush.name     IN {prob_bush_names} ~
                       AND sol_bush.name      IN {sol_bush_names} ~
                  ~:[~;AND prob_pic_bush.name IN {prob_pic_bush_names}~] ~
                  ~:[~;AND sol_pic_bush.name  IN {sol_pic_bush_names} ~] ~
                     SET prob_bush.accessed     = timestamp(), ~
                         sol_bush.accessed      = timestamp() ~
                  ~:[~;, prob_pic_bush.accessed = timestamp()~] ~
                  ~:[~;, sol_pic_bush.accessed  = timestamp()~] ~
                     MERGE (new:Mini_article    {name : {name}}) ~
                       ON CREATE SET new.created = timestamp(), ~
                                     new.title   = {title}, ~
                                     new.votes   = 0, ~
                                     new.uri     = \"/mini-article-not-initiated\"
                     MERGE (prob:Paragraph          {name : {problem_name}}) ~
                       ON CREATE SET prob.created=timestamp()~
                                        ~:[~;, prob.content={prob_content}~] ~
                       ON MATCH SET prob.accessed=timestamp() ~
                     MERGE (sol:Paragraph           {name : {solution_name}}) ~
                       ON CREATE SET sol.created=timestamp()~
                                         ~:[~;, sol.content={sol_content}~] ~
                       ON MATCH SET sol.accessed=timestamp() ~
                     MERGE (prob_pic:Picture:Visual {name : {prob_pic_name}}) ~
                       ON CREATE SET        prob_pic.path = {prob_pic_path}, prob_pic.created=timestamp() ~
                       ON MATCH SET prob_pic.accessed=timestamp() ~
                     MERGE (sol_pic:Picture:Visual  {name : {sol_pic_name}}) ~
                       ON CREATE SET         sol_pic.path = {sol_pic_path}, sol_pic.created=timestamp() ~
                       ON MATCH SET sol_pic.accessed=timestamp() ~
                ~:[~;MERGE (prob_pic_caption:Paragraph {name : {prob_pic_caption_name}}) ~
                       ON CREATE SET prob_pic_caption.created=timestamp()~
                                   , prob_pic_caption.content={prob_pic_caption_content} ~
                       ON MATCH SET prob_pic_caption.accessed=timestamp()~] ~
                ~:[~;MERGE (sol_pic_caption:Paragraph {name : {sol_pic_caption_name}}) ~
                       ON CREATE SET sol_pic_caption.created=timestamp()~
                                   , sol_pic_caption.content={sol_pic_caption_content} ~
                       ON MATCH SET sol_pic_caption.accessed=timestamp()~] ~
                     MERGE (new)              -[:PROBLEM ]-> (prob) ~
                     MERGE (new)              -[:VISUAL  ]-> (prob_pic) ~
                     MERGE (prob_pic)         -[:EXPLAINS]-> (prob) ~
                     MERGE (new)              -[:SOLUTION]-> (sol) ~
                     MERGE (new)              -[:VISUAL  ]-> (sol_pic) ~
                     MERGE (sol_pic)          -[:EXPLAINS]-> (sol) ~
               ~:[~; MERGE (prob)             -[:IS_ABOUT]-> (prob_bush)~] ~
               ~:[~; MERGE (sol)              -[:IS_ABOUT]-> (sol_bush) ~] ~
               ~:[~; MERGE (prob_pic)         -[:DEPICTS ]-> (prob_pic_bush)~] ~
               ~:[~; MERGE (sol_pic)          -[:DEPICTS ]-> (sol_pic_bush)~] ~
               ~:[~; MERGE (new)              -[:CAPTION ]-> (prob_pic_caption)~] ~
               ~:[~; MERGE (new)              -[:CAPTION ]-> (sol_pic_caption)~] ~
               ~:[~; MERGE (prob_pic_caption) -[:EXPLAINS]-> (prob_pic)~] ~
               ~:[~; MERGE (sol_pic_caption)  -[:EXPLAINS]-> (sol_pic)~] ~
                    RETURN new.name"
                    prob-pic-bush-names
                    sol-pic-bush-names
                    prob-pic-bush-names
                    sol-pic-bush-names
                    prob-pic-bush-names
                    sol-pic-bush-names
                    problem-description
                    solution-description
                    prob-pic-caption
                    sol-pic-caption
                    problem-description-bush-names
                    solution-description-bush-names
                    prob-pic-bush-names
                    sol-pic-bush-names
                    prob-pic-caption
                    sol-pic-caption
                    prob-pic-caption
                    sol-pic-caption)
               (append
                 `(("title" . ,title)
                   ("name" . ,name)
                   ("problem_name"        . ,problem-name)
                   ("solution_name"       . ,solution-name)
                   ("prob_pic_name"       . ,prob-pic-name )
                   ("prob_pic_path"       . ,problem-picture-path)
                   ("sol_pic_name"        . ,sol-pic-name)
                   ("sol_pic_path"        . ,solution-picture-path)
                   ("prob_bush_names" . ,problem-description-bush-names)
                   ("sol_bush_names"  . ,solution-description-bush-names))
                 (loop for l1 in (list problem-description solution-description prob-pic-caption sol-pic-caption prob-pic-caption-name sol-pic-caption-name) 
                       and param-name1 in 
                       '("prob_content" "sol_content" "prob_pic_caption_content" "sol_pic_caption_content" "prob_pic_caption_name" "sol_pic_caption_name") 
                       when l1 collect `(,param-name1 . ,l1))
                 (loop for l2 in (list prob-pic-bush-names sol-pic-bush-names)
                       and param-name2 in '("prob_pic_bush_names" "sol_pic_bush_names")
                       when l2 collect `(,param-name2 . ,(coerce l2 'simple-vector))))
               :commit-uri commit-uri :commit? t))))
    (lg :warn "Tried to insert-mini-article, but can't resolve problem or solution~%")))

;; Skeleton insert-mini-article
;;(insert-mini-article ""            ; title
;;                     #p"pictures/" ; problem picture
;;                     #p"pictures/" ; solution picture
;;                     :problem-description ""
;;                     :solution-description ""
;;                     :problem-description-bush-names '("")         
;;                     :solution-description-bush-names '("")         
;;                     :problem-name ""
;;                     :solution-name ""
;;                     :prob-pic-bush-names '("")
;;                     :sol-pic-bush-names '("")
;;                     :prob-pic-caption ""
;;                     :sol-pic-caption "")

(defun test-insert-mini-article ()
  (insert-mini-article "The first mini article"           ;title
                       #p"pictures/problem_picture.png"   ;problem picture
                       #p"pictures/solution_picture.jpg"  ;solution picture
                       :problem-description "Creating mini articles is hard. That is a problem. This is the problem description."
                       :solution-description "The solution is a huge function. This is the solution description."
                       :problem-description-bush-names '("Theory")
                       :solution-description-bush-names '("Software")
                       :problem-name "Mini Articles Are Hard To Create"
                       :solution-name "Make a large lisp function"
                       :prob-pic-bush-names '("Hardware" "Linear Bearing")
                       :sol-pic-bush-names '("Slicer" "Marlin")
                       :prob-pic-caption "This text is the caption of the problem picture. It should explain the problem picture."
                       :sol-pic-caption "The picture shown above this text is the solution picture. This is the caption of it, and should explain it well."))
      

(defun test-insert-another-mini-article (&key reset)
  (insert-mini-article "The second mini article"     ;title
                       #p"pictures/pic.jpg"          ;problem picture
                       #p"pictures/solution2.jpg"    ;solution picture
                       :problem-description "Having only one mini article makes it hard to do proper testing. This is the problem description of the second article."
                       :solution-description "This function makes another mini article. This is the solution description of the second article."
                       :problem-description-bush-names '("Software")
                       :solution-description-bush-names '("Mendel")
                       :problem-name "Only One Mini Article"
                       :solution-name "Another Mini Article"
                       :prob-pic-bush-names '("Huxley")
                       :sol-pic-bush-names '("Heatbed")
                       :prob-pic-caption "This is the prob-pic-caption of the second article."
                       :sol-pic-caption "This is the sol-pic-caption of the second article."))

(defun test-insert-third-article ()
  (insert-mini-article "Excessive Material Feed"                         ;title
                       #p"pictures/excessive-material-feed.jpg"          ;problem picture
                       #p"pictures/excessive-material-feed-solution.png" ;solution picture
                       :problem-description "Wall thicknesses are coming out too thick, objects have outside dimensions consistently slightly too large and holes are slightly too small. "
                       :solution-description "Reduce flow rate setting for perimeters. Both Skeinforge and Slic3r has these settings available to change."
                       :problem-description-bush-names '("Object Dimensions")
                       :solution-description-bush-names '("Slicer" "Slic3r" "Skeinforge")
                       :problem-name "Excessive Material Feed"
                       :solution-name "Reducing Perimeter Flow Rates"
                       :prob-pic-bush-names '("Printed Object")
                       :sol-pic-bush-names '("Slic3r")
                       :prob-pic-caption "Figure with too fat walls."
                       :sol-pic-caption "Screenshot from Slic3r. The setting to change is marked."))

(defun insert-all-mini-articles ()
    (test-insert-mini-article)
    (test-insert-another-mini-article)
    (test-insert-third-article)
    (parameter-names-mini-article))
;; Får 625 stycken (:ROW "Title") tillbaka
;; Det är exakt 5*5*5*5 = 625
;; Det är antal 
;; problem-description-bush-names * solution-description-bush-names * prob-pic-bush-names * sol-pic-bush-names
;; Något går snett när duplikater ska plockas bort
;; Får också problem med json-encodingen ibland när denna funktionen körs.
(defun parameter-names-mini-article ()
  (insert-mini-article "Title"
                       #p"pictures/problem_picture.png"
                       #p"pictures/solution_picture.jpg"
                       :problem-description "Problem-description"
                       :solution-description "Solution-description"
                       :problem-description-bush-names '("Theory" "Software" "Hardware" "Model" "Printed Object")
                       :solution-description-bush-names '("Theory" "Software" "Hardware" "Model" "Printed Object")
                       :problem-name "Problem-name"
                       :solution-name "Solution-name"
                       :prob-pic-bush-names '("Theory" "Software" "Hardware" "Model" "Printed Object")
                       :sol-pic-bush-names '("Theory" "Software" "Hardware" "Model" "Printed Object")
                       :prob-pic-caption "Prob-pic-caption"
                       :sol-pic-caption "Sol-pic-caption"))

;; Två transaction-requests får neo4j att hänga sig. 
;; Inget felmedellande. 

(defun delete-mini-article (name)
  (cypher-query 
    "MATCH (ma:Mini_article {name : {name}})-[r]->() ~
     DELETE r, ma"
    `(("name" . ,name))))

(defun vote-for (name)
  (cypher-query 
    (format 
      nil
      "MATCH (ma:Mini_article {name : {name}}) ~
       SET ma.votes=ma.votes+1")
    :params `(("name" . ,name))))

(defun ma-titles ()
  (simple-extract
    (cypher-query
      (format
        nil
        "MATCH (ma:Mini_article) RETURN ma.title"))))

(defun ma-exists? (name)
  (ca :data 
      (cypher-query 
        "MATCH (a:Mini_article {name : {name}}) RETURN a.name"
        :params `(("name" . ,name)))))

(defun children-of (bush-name)
  (ca :data
      (cypher-query 
          (format nil "MATCH (ocat:Category {name : {name}})<-[:IS]-(cat:Bush) ~
                       RETURN DISTINCT cat.name ORDER BY cat.name")
        :params `(("name" . ,bush-name)))))

(defun update-property-for-all-mas (property value)
  (cypher-query
    (format
      nil
      "MATCH (ma:Mini_article) SET ma.~a={value}" property)
    :params `(("value" . ,value))))

(defun name-and-label-exist? (name label)
  (ca :data 
      (cypher-query 
        (format nil "MATCH (b:~a {name:{name}}) RETURN b.name" label) 
        :params `(("name" . ,(name-conventions name))))))

(defun categories-query (categories)
  (ca :data
      (cypher-query 
        (if categories
          (format nil "MATCH (ocat:Category)<-[:IS]-(cat:Bush) ~
                       WHERE ocat.name IN {categories} ~
                       RETURN DISTINCT cat.name, ocat.name ORDER BY ocat.name")
          (format nil "MATCH (reprap:Reprap)<-[:IS]-(cat:Category) ~
                       RETURN DISTINCT cat.name ORDER BY cat.name"))
        :params (when categories `(("categories" . ,(coerce (listify categories) 'simple-vector)))))))

