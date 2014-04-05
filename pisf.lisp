(in-package :pisf)

(defparameter *pisf-acceptor* nil)

(defun path->uri (path)
  (concatenate 'string "/" (namestring path)))

;; Pisf should only deal with relative pathnames.
;; Database should containt only relative pathnames
(defun serve-static (path &optional (uri (namestring (make-pathname ; to make utf-8 and spaces work...
                                                       :name (url-encode (pathname-name path)) 
                                                       :defaults (path->uri path)))))
  (lg :info "Starting to serve static ~a at ~a~%" (with-source path) (if uri uri (path->uri path)))
  (push (create-static-file-dispatcher-and-handler
          uri (with-source path)) 
        *dispatch-table*))

(defun function-serve (uri fn)
  (lg :info "Starting to serve function at ~a~%" uri)
  (push (create-prefix-dispatcher uri fn) *dispatch-table*))

;; When *ma-control-string* is developed, declare new vaiebles straight
;; into cypher query
;; This function should always be used directly after insert-mini-article
(defun serve-mini-article (name &key (serve-pictures? t))
  (bind-cypher-returns-singles 
    ("MATCH ~
      (mini_article:Mini_article {name:{name}})-[:SOLUTION]->(solution:Paragraph), ~
      (mini_article)-[:PROBLEM]->(problem:Paragraph), ~
      (mini_article)-[:VISUAL]->(prob_pic:Picture)-[:EXPLAINS]->(problem), ~
      (mini_article)-[:VISUAL]->(sol_pic:Picture) -[:EXPLAINS]->(solution) ~
      OPTIONAL MATCH ~
      (mini_article)-[:CAPTION]->(prob_caption:Paragraph)-[:EXPLAINS]->(prob_pic), ~
      (mini_article)-[:CAPTION]->(sol_caption:Paragraph) -[:EXPLAINS]->(sol_pic) ~
      RETURN ~
      mini_article.title, ~
      mini_article.name, ~
      prob_pic.name, ~
      sol_pic.name, ~
      prob_pic.path, ~
      sol_pic.path, ~
      problem.content, ~
      solution.content, ~
      prob_caption.content, ~
      sol_caption.content" :params `(("name" . ,(name-conventions name))))
      (if (not mini_article.title) 
        (lg :warn "couldn't serve ~a. Ma-pattern not found in db~%" name)
        (progn (when serve-pictures?
                 (serve-static prob_pic.path) 
                 (serve-static (small-version prob_pic.path))
                 (serve-static sol_pic.path)
                 (serve-static (small-version sol_pic.path)))
               (let* ((filename (concatenate 'string (my-string->word mini_article.title) ".html"))
                      (filepath_relative (concatenate 'string "mini_articles/" filename))
                      (ma-uri (title->uri mini_article.title)))
                 (with-open-file (s (with-source filepath_relative)
                                    :direction :output
                                    :if-exists :supersede
                                    :if-does-not-exist :create)
                   (lg :info "Writing file ~a~%" (with-source filepath_relative))
                   (format s *ma-control-string* 
                           mini_article.title
                           mini_article.title
                           problem.content
                           prob_pic.path
                           (small-version prob_pic.path)
                           prob_pic.name
                           prob_pic.name
                           (aif prob_caption.content it "Default problem picture caption.")
                           solution.content
                           sol_pic.path
                           (small-version sol_pic.path)
                           sol_pic.name
                           sol_pic.name
                           (aif sol_caption.content it "Default solution picture caption.")
                           mini_article.name)
                   (serve-static filepath_relative (format nil "/~a" (url-encode (subseq ma-uri 1)))) ; escapes special characters...
                   (cypher-query ;; Tell the database we're serving the mini article
                     "MATCH (ma:Mini_article {title : {title}}) SET ma.uri={uri}"
                     :params `(("title" . ,mini_article.title)
                               ("uri" . ,ma-uri)))) ma-uri)))))

(defun serve-all-mini-articles ()
  (bind-cypher-returns-and-loop 
    ("MATCH ~
      (mini_article:Mini_article)-[:SOLUTION]->(solution:Paragraph), ~
      (mini_article)-[:PROBLEM]->(problem:Paragraph), ~
      (mini_article)-[:VISUAL]->(prob_pic:Picture)-[:EXPLAINS]->(problem), ~
      (mini_article)-[:VISUAL]->(sol_pic:Picture) -[:EXPLAINS]->(solution) ~
      OPTIONAL MATCH ~
      (mini_article)-[:CAPTION]->(prob_caption:Paragraph)-[:EXPLAINS]->(prob_pic), ~
      (mini_article)-[:CAPTION]->(sol_caption:Paragraph) -[:EXPLAINS]->(sol_pic) ~
      RETURN ~
      mini_article.title, ~
      mini_article.name, ~
      prob_pic.name, ~
      sol_pic.name, ~
      prob_pic.path, ~
      sol_pic.path, ~
      problem.content, ~
      solution.content, ~
      prob_caption.content, ~
      sol_caption.content") 
      (serve-static prob_pic.path)
      (serve-static (small-version prob_pic.path))
      (serve-static sol_pic.path)
      (serve-static (small-version sol_pic.path))
      ;; Create static file
      (let* ((filename (concatenate 'string (my-string->word mini_article.title) ".html"))
             (filepath_relative (concatenate 'string "mini_articles/" filename))
             (ma-uri (title->uri mini_article.title)))
        (with-open-file (s (with-source filepath_relative)
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
          (lg :info "Writing file ~a~%" (with-source filepath_relative))
          (format s *ma-control-string* 
                  mini_article.title
                  mini_article.title
                  problem.content
                  prob_pic.path
                  (small-version prob_pic.path)
                  prob_pic.name
                  prob_pic.name
                  (aif prob_caption.content it "Default problem picture caption.")
                  solution.content
                  sol_pic.path
                  (small-version sol_pic.path)
                  sol_pic.name
                  sol_pic.name
                  (aif sol_caption.content it "Default solution picture caption.")
                  mini_article.name)
          (serve-static filepath_relative (format nil "/~a" (url-encode (subseq ma-uri 1)))) ; escapes special characters. They should not get into db in first place
          (cypher-query ;; Tell the database we're serving the mini article
            "MATCH (ma:Mini_article {title : {title}}) SET ma.uri={uri}"
             :params `(("title" . ,mini_article.title)
                       ("uri" . ,ma-uri)))))))

;; Mini article html-template
(defparameter *ma-control-string* 
  (generate-standard-page
    (:title "~a")               ; The title
    (:h1 "~a")                  ; The title
    (:h3 "Problem")
    (:p "~a")                   ; The problem description
    (:figure
      (:a :href (path->uri "~a")            ; The link from problem picture (should be uri?)
       (:img :src (path->uri "~a")          ; The problem picture (should be uri?)
        :alt "~a"))             ; The prob_pic.name
      (:figcaption "~a: ~a"))   ; The problem visual name + problem figcaption
    (:h3 "Solution")
    (:p "~a")                   ; The solution description
    (:figure
      (:a :href (path->uri "~a")            ; The link from solution picture (should be uri?)
       (:img :src (path->uri "~a")          ; The solution picture (should be uri?)
        :alt "~a"))             ; The sol_pic.name
      (:figcaption "~a: ~a"))   ; The solution visual name + solution figcaption
    (:div :id "BigButton" :class "bottom" ;; submit form would save a lot of button design work
     (:a :href "/vote?name=~a" ; must get mini_article.name not title
      "Good article!"))))

     ;;(:a :href (format nil "/vote?name=~a" (escape-string "~a")) ; must get mini_article.name not title

(defun initiate-web-app (&optional (port 8080))
  (serve-static "style.css")
  (serve-static "pictures/Checkmark2.jpg")
  (serve-static "style.css" "/mini_articles/style.css") ; Same css file to all
  (serve-static "pictures/teardrop.jpg")
  (update-property-for-all-mas "uri" "/mini-article-not-initiated") ; This uri should be served before its used here.
  (serve-all-mini-articles)
  (setf *pisf-acceptor* (start (make-instance 'easy-acceptor :port port))))
  
;; Both mas-query and categories-query should start in the selected :Bush node

(defun mas-query (categories)
  (lg :info "mas-query got the relevant leaves ~w~%" categories)
  (ca :data
    (cypher-query 
      (if categories ; If videos are added, then :IS_ABOUT|DEPICTS must get the video-relation added
        ;; Counting number of duplicate hits on each ma will give relevance
        (format nil "MATCH (cat:Bush) ~
                     WHERE cat.name IN {categories} ~
                     MATCH (cat) <-[:IS*0..]- (cat2:Bush) <-[:IS_ABOUT|DEPICTS]- () <-- (ma:Mini_article) ~
                     RETURN DISTINCT ma.title, ma.uri, ma.votes ORDER BY ma.votes DESC")
        "MATCH (ma:Mini_article) RETURN ma.title, ma.uri, ma.votes ORDER BY ma.votes DESC")
      :params (when categories `(("categories" . ,(coerce (listify categories) 'simple-vector)))))))

(defun pair? (lst)
  (stringp (first lst)))

(defun checked? (pair)
  (second pair))

(defun name-extract (pair)
  (first pair))

(defun none-checked? (checkbox-tree)
  (cond
    ((null checkbox-tree) t)
    ((pair? (first checkbox-tree)) 
     (if (checked? (first checkbox-tree)) 
       nil
       (none-checked? (rest checkbox-tree))))
    (t(or (none-checked? (first checkbox-tree))
          (none-checked? (rest checkbox-tree))))))

;; Tags like :p and :input won't work in loops or in other macros unless they have another
;; with-html-output (that is a HTM) around them. All macros that are defined in
;; macrolet by with-html-output (that is fmt, esc, str, htm) will still work in
;; a loop.
;; 
;; Inside a tag (like inside (:p )), only strings are printed to stream.
;; Numbers and such must be made to strings with for example fmt.
;; 
;; After a :value tag, cl-who looks for a string. If it finds nil, the
;; :value tag won't be used.
;; 
;; The return values of with-html-output are a bit hard to predict
;; (probably the last princed thing), but it
;; prints everything to the stream reliably.

(defun make-checkboxes (checkbox-tree &optional (indents 0))
    (labels ((f (checkbox-tree indents father)
               (cond 
                 ((null checkbox-tree))
                 ((pair? (first checkbox-tree))
                  (with-html-output
                    (*standard-output* nil :prologue nil :indent t)
                    (loop repeat indents do
                          (str "&nbsp")) ; could have (esc "tab-sign")..
                    (:input :type "checkbox" :name "checked" 
                     :checked (if (checked? (first checkbox-tree)) "checked" nil)
                     :value (format nil "~w" (list (name-extract (first checkbox-tree)) (list father))) (str (name-extract (first checkbox-tree))))(:br))
                    (if (and (second checkbox-tree) (not (pair? (second checkbox-tree))))
                      (progn
                        (f (second checkbox-tree) (+ 1 indents) (name-extract (first checkbox-tree)))
                        (f (rest (rest checkbox-tree)) indents father))
                      (f (rest checkbox-tree) indents father)))
                 ((null (first checkbox-tree))
                  (with-html-output
                    (*standard-output* nil :prologue nil :indent t)
                    (loop repeat (+ 2 indents) do
                          (str "&nbsp"))
                    (str "Bottom of tag hierarchy")(:br))
                  (f (rest checkbox-tree) indents father)))))
      (f checkbox-tree indents "Reprap")))

(defun update-tree (checkbox-tree)
  (if (and (listp checkbox-tree) (null checkbox-tree)) ;; given? does not exist for argument (Hardware) ???
    (categories-query '())
    (do* 
      ((l (length checkbox-tree))
       (index 0 (incf index))
       (checked-ones 0 (incf checked-ones))
       (looked-at (first checkbox-tree) (nth index checkbox-tree))
       (next-out (second checkbox-tree) (nth (+ 1 index) checkbox-tree)))
      ((= checked-ones l) checkbox-tree)
      ;;(lg :info "Update-tree looking at the pair/list ~a, index is ~a~%" looked-at index)
      (cond 
        ((and (pair? looked-at) (checked? looked-at) ;; insert new categories
              (or (pair? next-out) (= checked-ones (- l 1))))
         (let ((children (children-of (name-extract looked-at))))
           (if children
             (insert-after checkbox-tree index children)
             (insert-after checkbox-tree index nil))) ;; No sub-tags found
         (incf index))
        ((and (pair? looked-at) (not (checked? looked-at)) 
              (< checked-ones (- l 1)) 
              (not (pair? next-out)));; a previously interesting category is not interesting anymore
         (delete-nth checkbox-tree (+ 1 index))
         (decf l))
        ((not (pair? looked-at))
         (update-tree looked-at))))))

(defun father-extract (cp)
  (first (second cp)))

;;(lg :info "f running with ct ~w and c ~w~%" ct c)
;;(lg :info "f found end~%"))
;;(lg :info "f found to ~w be inserted~%" (name-extract (first ct)))
;;(lg :info "f found a non-pair first~%")
;;(lg :info "f found nothing of interest, recursing~%")
(defun handle-checked (checkbox-tree checked)
  (labels ((f (ct c father)
             (cond
               ((endp ct)
                ) 
               ((and (pair? (first ct)) ;; element was checked by user now
                     (not (checked? (first ct)))
                     (find (name-extract (first ct)) c :test #'(lambda (name c-and-f)
                                                                 (and (string= father (father-extract c-and-f))
                                                                      (string= (first c-and-f) name)))))
                ;;(if (find (name-extract (first ct)) handled :test #'string=))
                (insert-after (first ct) 0 t)
                (if (not (pair? (second ct)))
                  (progn
                    (f (second ct) c (name-extract (first ct)))
                    (f (rest (rest ct)) c father))
                  (f (rest ct) c father)))
               ((and (pair? (first ct)) ;; element was unchecked by user now
                     (checked? (first ct))
                     (not (find (name-extract (first ct)) c :test #'(lambda (name c-and-f)
                                                                 (and (string= father (father-extract c-and-f))
                                                                      (string= (first c-and-f) name))))))
                (delete-nth (first ct) 1)
                (delete-nth ct 1) ; sub-tags collapses and disappears if father tag was unchecked
                (f (rest ct) c father))
               ((pair? (first ct))
                (if (not (pair? (second ct)))
                  (progn
                    (f (second ct) c (name-extract (first ct)))
                    (f (rest (rest ct)) c father))
                  (f (rest ct) c father))))))
    (f checkbox-tree checked "Reprap")
    checkbox-tree))

(defun url-encode-rec (val)
  (typecase val
    (cons
     (mapcar #'url-encode-rec val))
    (string 
     (url-encode val))
    (number val)))

(defun encode-rec (val)
  (typecase val
    (cons
     (mapcar #'encode-rec val))
    (string 
     (escape-string (url-encode val)))
    (number val)))

;; It's important to first build the url (with url encoding) first, 
;; then to escape html characters, to get predictable links

(defun compute-link-str (ctrl-str &rest ctrl-args)
  (let ((*print-pretty* nil))
    (apply #'format nil ctrl-str (encode-rec ctrl-args))))

;; More intuitive but slower version creating uglier strings (dublequotes gets
;; html-escaped instead of just \")
(defun compute-link-str2 (ctrl-str &rest ctrl-args)
  (let ((*print-pretty* nil))
    (escape-string (url-encode (apply #'format nil ctrl-str ctrl-args)))))

(defun compute-redirect-str (ctrl-str &rest ctrl-args)
  (let ((*print-pretty* nil))
    (apply #'format nil ctrl-str (url-encode-rec ctrl-args))))

(defparameter *two-or-more-newlines-regex* '(:sequence (:alternation #\Newline (:sequence #\Return #\Newline)) 
                                                       (:greedy-repetition 1 nil 
                                                          (:sequence (:greedy-repetition 0 nil :whitespace-char-class) 
                                                                     (:alternation #\Newline (:sequence #\Return #\Newline))))))
(defparameter *web-adress-regex* '(:alternation
                                    (:sequence :whitespace-char-class #\w #\w #\w #\. 
                                     (:non-greedy-repetition 1 nil :non-whitespace-char-class)
                                     #\.
                                     (:non-greedy-repetition 1 nil :non-whitespace-char-class)
                                     (:alternation :whitespace-char-class :end-anchor))
                                    (:sequence :whitespace-char-class #\h #\t #\t #\p (:greedy-repetition 0 1 #\s) #\:
                                     (:non-greedy-repetition 1 nil :non-whitespace-char-class)
                                     #\.
                                     (:non-greedy-repetition 1 nil :non-whitespace-char-class)
                                     (:alternation :whitespace-char-class :end-anchor))))

(defun escape-except-links (text)
  (with-output-to-string (s)
    (let ((previous-end 0))
      (cl-ppcre:do-matches (start end *web-adress-regex* text)
        (write-string (escape-string (subseq text previous-end start)) s)
        (when (string= (subseq text (+ 1 start) (+ 8 start)) "http://")
          (incf start 7))
        (when (string= (subseq text (+ 1 start) (+ 9 start)) "https://")
          (incf start 8))
        (format s " <a href=http://~a>~a</a> " (escape-string (subseq text (+ start 1) (- end 1))) (escape-string (subseq text (+ start 1) (- end 1))))
        (setf previous-end end))
      (write-string (escape-string (subseq text previous-end)) s)))) ; The text after last web-adress
  

;; This could be a full markdown parser. 3bmd is a cl library (although slow)
;; for handling that parsing
(defun escape-text (text)
  (regex-replace *two-or-more-newlines-regex* 
                 (escape-except-links text) 
                 "</p><p>"))

       ;;(:input :type "submit" :value "Filter!" :class "btn"))
(define-easy-handler (pisf :uri "/print-issue-solution-filter") ((checked :parameter-type '(list string)) (checkbox-tree :parameter-type #'read-from-string))
  (let* ((checked (mapcar #'read-from-string checked)) ; Should maybe work with cookies instead of these monster arguments?
         (new-tree (update-tree (handle-checked checkbox-tree checked)))
         (mas-lists (mas-query (relevant-leaves new-tree))))
    (generate-standard-page
      (:title "Print Issue Solution Filter")
      (:script (str (ps
                      (defun update-slider (slide-amount)
                        (defvar display (chain document (get-element-by-id "chosen")))
                        (setf (@ display inner-h-t-m-l) slide-amount))))) ; end of script
      (:strong (fmt "HITS: ~d" (length mas-lists)))(:br)
      (:form :action (compute-link-str "/print-issue-solution-filter?checkbox-tree=~w" new-tree) :method "post" :id "addform"
       (make-checkboxes new-tree)
       (submit :value "Filter!"))
      (:h2 (fmt "List of ~d matching articles:" (length mas-lists)))
      (:div :id "chart" ; not yet used for css styling
       (:ol
         (do* ((mas-list (pop mas-lists) (pop mas-lists))
               (ma.title (pop mas-list) (pop mas-list))
               (ma.uri (pop mas-list) (pop mas-list))
               (ma.votes (pop mas-list) (pop mas-list)))
           ((null ma.title))
           (when (or (string= "/mini-article-not-initiated" ma.uri) (null ma.uri))
             (lg :warn "Linking to not initiated mini article ~s, with uri ~s on front page~%" ma.title ma.uri))
           (htm
             (:li 
               (:a :href  ma.uri 
                (esc ma.title))
               (fmt " with ~d votes" ma.votes)))))) ;; and x relevance
      (:p "Know a solution that's not here? Make it available to others " 
       (:a :href "/new-mini-article" "here")))))


;; If safaguard against Windows putting backspaces in paths would be needed:
;;      (when (search "Windows" (user-agent) :test 'char-equal) ; user-agent will only work if *request* is bound
;;        (setq file-name (regex-replace ".*\\\\" file-name "")))

(defparameter *csv-regex* "\\s*([0-9a-zA-Z-]+)(\\s[0-9a-zA-Z-]+)*(\\s*,\\s*([0-9a-zA-Z-]+)(\\s[A-Za-z0-9-]+)*)*\\s*")
(defparameter *csv-regex-explanation* "Tag1, Tag2, ... (Tag names may use characters a-z, A-Z, 0-9 and hyphen)")

(defparameter *param-names* '("title" 
                              "problem" 
                              "prob_pic"
                              "prob_pic_caption"
                              "solution"
                              "sol_pic"
                              "sol_pic_caption"))

(defparameter *it-strs* '("title"
                          "problem description"
                          "problem picture"
                          "problem picture caption"
                          "solution description"
                          "solution picture"
                          "solution picture caption"))

(defparameter *which-labels* '("Mini_article" 
                               "Paragraph" 
                               "Picture" 
                               "Paragraph" 
                               "Paragraph" 
                               "Picture" 
                               "Paragraph"))

(defun title? (which)
  (zerop which))

(defun paragraph? (which)
  (find which '(1 3 4 6) :test #'=))

(defun picture? (which)
  (find which '(2 5) :test #'=))

(defun which->label (which)
  (nth which *which-labels*))

(defun which->it-str (which)
  (nth which *it-strs*))

;; Should be called with names of previously added nodes as parameters
;; Must send names of previously added nodes as parameters and something that
;; indicate which node is to be inserted
(define-easy-handler (new-mini-article :uri "/new-mini-article") 
                     ((names :parameter-type #'read-from-string))
  (lg :info "new-mini-article called with names=~w~%" names)
  (unless (given? names)
    (setf names '(nil nil nil nil nil nil nil)))
  (generate-standard-page (:title "Add a new mini article")
    (:h1 "Add a new mini article")
    ;; Would it be a good idea to store names in a hash-table with session
    ;; identifiers, and store the identifier in the users machine with a cookie?
    (:form :action (compute-link-str "/add-node?names=~w" names) :method "post" :id "addform"
     (:ul
       (loop for name in names
             and i from 0
             and it-str in *it-strs*
             do (htm (:li :class (if (given? name) "hasName" "noName") ;; hasName/noName for checkbox picture
                      (:button :name "which" :value i :type "submit" (fmt "Add a ~a" it-str)))))))
    (:form :action (if (every #'identity names) "/build-mini-article" nil) :method "post"
     (unless (every #'identity names) (str "If all pictures and paragraphs are added, this button will publish the mini article. "))
     (:button :name "names" :value (compute-link-str "~w" names) :type "submit" :class "btn" 
      (str (if (every #'identity names) "Build mini article!" "Mini article not complete"))))
    ;; The whole preview part should be singled out into a function
    (let ((givens (loop for name in (rest names) ; Don't collect title, we haven't put it in db
                        and i from 1
                        when name
                        collect i)))
      (htm
        (:p :class "top" (:strong "Preview of Mini Article")))
      (if (null givens) ; Then only title was maybe given
        (aif (first names) (htm (:h1 (str it))) (htm (:p "Nothing here yet.")))
        (let* ((params (loop for given in givens
                             collect (cons (nth given *param-names*) (nth given names))))
               (query (do* ; This loop builds the query based on which names are given
                        ((given (pop givens) (pop givens))
                         (param-name (nth given *param-names*) (nth given *param-names*))
                         (next-match-str (format nil "(~a:~a {name : {~a}})" 
                                                 param-name (if (picture? given) "Picture" "Paragraph") param-name)
                                         (format nil "(~a:~a {name : {~a}})" 
                                                 param-name (if (picture? given) "Picture" "Paragraph") param-name))
                         (next-ret-str (format nil "~a.~a" param-name (if (picture? given) "path" "content"))
                                       (format nil "~a.~a" param-name (if (picture? given) "path" "content")))
                         (match-acc (cons next-match-str nil) 
                                    (cons next-match-str match-acc))
                         (ret-acc (cons next-ret-str nil)
                                  (cons next-ret-str ret-acc))) 
                        ((endp givens) (format nil "MATCH ~{~a~^, ~} RETURN ~{~a~^, ~}" match-acc ret-acc))))
               (cyph-ret (cypher-query query :params params))
               (problem.content (extract-value "problem.content" cyph-ret))
               (prob_pic.path (extract-value "prob_pic.path" cyph-ret))
               (prob_pic_caption.content (extract-value "prob_pic_caption.content" cyph-ret))
               (solution.content (extract-value "solution.content" cyph-ret))
               (sol_pic.path (extract-value "sol_pic.path" cyph-ret))
               (sol_pic_caption.content (extract-value "sol_pic_caption.content" cyph-ret)))
            (htm
              (:div :class "bottom"
              (:h1 (str (aif (first names) it "Not title yet.")))
              (:h3 "Problem")
              (:p (str (aif problem.content it "No problem description yet.")))
              (if prob_pic.path
                (htm
                  (:figure
                    (:a :href (path->uri prob_pic.path)
                     (:img :src (path->uri (small-version prob_pic.path)) ; The problem picture (should be uri?)
                      :alt "The problem picture name"))             ; The prob_pic.name
                    (:figcaption (str (aif prob_pic_caption.content it "No problem picture caption yet.")))))
                "No Problem Picture yet. Adding caption first is ok, but it won't show up here before picture is added.")
              (:h3 "Solution")
              (:p (str (aif solution.content it "No solution description yet.")))
              (if sol_pic.path
                (htm
                  (:figure
                    (:a :href (path->uri sol_pic.path)
                     (:img :src (path->uri (small-version sol_pic.path)) ; The problem picture (should be uri?)
                      :alt "The solution picture name"))             ; The prob_pic.name
                    (:figcaption (str (aif sol_pic_caption.content it "No solution picture caption yet.")))))
                "No Solution Picture yet. Adding caption first is ok, but it won't show up here before picture is added."))))))))

 (defun which-column? (str cyph-ret)
   (position str (rest (assoc :columns cyph-ret)) :test #'string=))
 
 (defun extract-value (name cyph-ret)
   (awhen (which-column? name cyph-ret)
     (nth it (second (assoc :data cyph-ret)))))
          
  

;; Navigating between two different ways to call handle-name
(define-easy-handler (add-node :uri "/add-node") ((which :parameter-type 'integer)
                                                  (names :parameter-type #'read-from-string))
  (lg :info "add-node called with names=~w and which=~w~%" names which)
  (let* ((it-str (which->it-str which))
         (title-header (format nil "Add a ~:(~a~) to the Database" it-str))) ; Upcase first character in every word
    (generate-standard-page (:title (str title-header))
                            (:h1 (str title-header))
                            (cond
                              ((title? which) 
                               (title-form))
                              ((paragraph? which) 
                               (content-form :it-str it-str))
                              ((picture? which) 
                               (picture-form :it-str it-str))
                              (t ; My first error page
                               (htm
                                 (:p (fmt "parameter which should be < 7 but > 0. Now it is ~w" which))))))))

(define-easy-handler (nodepage :uri "/nodepage" :default-parameter-type 'string) (label name)
  (lg :info "nodepage called with label=~w, name=~w~%" label name)
  (string-case (label)
    ("Picture"
     (bind-cypher-returns-singles ("MATCH (p:Picture {name : {name}}) RETURN p.path" :params `(("name" . ,(name-conventions name))))
       (generate-standard-page (:title (str name))
         (:h1 (str name))
         (:figure
           (:a :href (path->uri p.path) ; Assumes picture is already being served. Node should maybe have a flag about this, like a "/picture-not-initiated" uri that is set at initiate-web-app
            (:img :src (path->uri (namestring (small-version p.path)))
             :alt name))
           (:figcaption (fmt "This picture already exist in the database with the name ~a" name))))))
    ("Paragraph"
     (generate-standard-page (:title (str name))
       (:h1 (str name))
       (bind-cypher-returns-singles ("MATCH (p:Paragraph {name : {name}}) RETURN p.content" :params `(("name" . ,(name-conventions name))))
         (htm
           (:p (str p.content))))))))

;; Move image to pictures/
;; create 500 px wide version
;; serve them both
(defun handle-image (post-parameter)
  (when (consp post-parameter)
    (destructuring-bind (path file-name #1=#:ignore) 
        post-parameter
    (declare (ignore #1#))
    (lg :info "Handling picture ~a with path ~a~%" file-name path)
      (let* ((new-relative-path (namestring (unique-name (format nil "pictures/~a" file-name))))
             (new-absolute-path (merge-pathnames new-relative-path *source-pathname*))
             (small-absolute-path (add-small new-absolute-path)))
        (rename-file path new-absolute-path)
        (sb-ext:run-program "/usr/bin/env convert" ; Very very system specific here...
                            `("-resize" "500" 
                              ,(namestring new-absolute-path)
                              ,(namestring small-absolute-path)))
        new-relative-path))))

        ;(serve-static new-relative-path)
        ;(serve-static (small-version new-relative-path))

;; A picture is now in the tmp-folder. Before we do anything else, we should
;; check if its name is already in the database.
;;
;; If picture doesn't exist in db, I want to put it in /pictures/, generate the
;; small version there, and generate the new relpath. Finally I want to put it into db.
;; 
;; If it already existed in db, I want to either, just go back to
;; /new-mini-article and move on, or I want to handle the image and set a new
;; name. This is the only occasion where new-name will be received.
;; So if new-name was supplied, we don't want to handle the picture again.
;;
;; Start serving it directly. Need that for previw
(define-easy-handler (receive-picture :uri "/receive-picture") ((names :parameter-type #'read-from-string)
                                                                (which :parameter-type 'integer)
                                                                path-data ; can be filled with path to already handled image or tmp-file data for sending to handle-image
                                                                (new-name :parameter-type 'string)
                                                                (is-about :parameter-type '(list string)))
  (let* ((db-name (if (given? new-name) (name-conventions new-name)
                    (path-data->db-name path-data)))
         (exists-in-db? (name-and-label-exist? db-name "Picture"))
         (it-str (which->it-str which)))
    (setf (nth which names) db-name) ; names is now updated
    (cond 
      (exists-in-db?
        (generate-standard-page (:title "Name already exists")
          (:p (fmt "The ~a name already exists in database " it-str)
           "with the following " 
           (:a :href (compute-link-str "/nodepage?label=~a&name=~a" 
                             "Picture" 
                             db-name) "content")
           ". If you want to use your own content, give it a new name and resubmit, otherwise "
           "just press no.")
          (:form :action (compute-link-str "/receive-picture?which=~w&path-data=~w&names=~w&is-about=~w"
                                 which (if (given? new-name) path-data (handle-image path-data)) names is-about) 
           :method "post" 
           (name)
           (submit))
          (:form :action "/new-mini-article" :method "post" ; Picture is not handled but already in db
           (:button :name "names" :value (compute-link-str "~w" names) :type "submit" "No")))) ; User is done
      ((given? new-name) ; picture is already handled but not yet in db
       (insert-picture path-data is-about :name new-name)
       (insert-picture (namestring (small-version path-data)) is-about
                         :name (concatenate 'string db-name "_small"))
       (redirect (compute-redirect-str "/new-mini-article?names=~w" names))) ; User is done
      (t ; picture is not yet handled and not yet in db
       (let ((img-relpath (strip-relpath-meta-count ; in case there was a picture in folder with this name afterall
                            (namestring (handle-image path-data)))))
         (if-returns-are-ok
           (insert-picture img-relpath is-about)
           (insert-picture (namestring (small-version img-relpath)) is-about
                           :name (concatenate 'string db-name "_small"))
           (progn
             (serve-static img-relpath)
             (serve-static (small-version img-relpath))
             (redirect (compute-redirect-str "/new-mini-article?names=~w" names))))))))) ; User is done

(define-easy-handler (receive-paragraph :uri "/receive-paragraph") ((names :parameter-type #'read-from-string)
                                                                    (which :parameter-type 'integer)
                                                                    (new-name :parameter-type 'string)
                                                                    content
                                                                    (is-about :parameter-type '(list string)))
  (let* ((label (which->label which))
         (db-name (if (given? new-name) (name-conventions new-name)
                     (paragraph->name content)))
         (exists-in-db? (name-and-label-exist? db-name label))
         (it-str (which->it-str which)))
    (setf (nth which names) db-name) ; names is now updated
    (cond 
      (exists-in-db?
        (generate-standard-page (:title "Name already exists")
          (:p (fmt "The ~a name ~s already exists in database " it-str db-name)
           "with the following " 
           (:a :href (compute-link-str "/nodepage?label=~a&name=~a" 
                             "Paragraph" 
                             db-name) "content")
           ". If you want to use your own content, give it a new name and resubmit, otherwise "
           "just press no.")
          (:form :action (compute-link-str "/receive-paragraph?which=~w&content=~w&names=~w&is-about=~a"
                                 which content names is-about) 
           :method "post" 
           (name)
           (submit))
          (:form :action "/new-mini-article" :method "post" 
           (:button :name "names" :value (compute-link-str "~w" names) :type "submit" "No")))) ; User is done
      (t ; paragraph not yet db and has unique name
         (if-returns-are-ok
           (insert-paragraph (escape-text content) is-about :name db-name)
           (redirect (compute-redirect-str "/new-mini-article?names=~w" names)))))))

(define-easy-handler (receive-title :uri "/receive-title") ((names :parameter-type #'read-from-string)
                                                            (new-name :parameter-type 'string))
  (if (name-and-label-exist? new-name "Mini_article")
    (generate-standard-page (:title "Exists")
      (:p "The Mini Article with title ~s already exists in the database with the following content: "
       (:a :href (title->uri new-name) (str new-name)) ; Want a more robust uri here...
        ". Please choose another title: ")
       (title-form)) ; redirect don't need html-escaping!
      (redirect (compute-redirect-str "/new-mini-article?names=~w" (cons new-name (rest names))))))
  
(defun small-pic (pic-paths)
  (second pic-paths))

(defun original-pic (pic-paths)
  (first pic-paths))

(defparameter *page-with-only-back-script* 
  "<!DOCTYPE html> <script type='text/javascript'>history.back()</script>") ; Doesn't refresh page
 ;  "<!DOCTYPE html> <script type='text/javascript'>window.location.replace(document.referrer)</script>")
;  ^ Doesn't send post-parameters

(define-easy-handler (vote :uri "/vote") (name)
  (when (ma-exists? name)
    (vote-for name))
  (redirect "/print-issue-solution-filter"))

(define-easy-handler (tag-added :uri "/tag-added") (name super-categories sub-categories 
                                                         (names :parameter-type #'read-from-string)
                                                         (which :parameter-type 'integer))
  (let ((super-categories (my-string->list super-categories))
        (sub-categories (my-string->list sub-categories)))
  (lg :info "Creating bush node with name: ~s~%super-categories: (~{~s~})~%sub-categories: (~{~s~})~%" name super-categories sub-categories)
  (create-bush-node name super-categories :subs sub-categories ) 
                    :cat-or-part (if sub-categories "Category" "Part")) ; my-string->list makes nil from empty strings
  (redirect 
    (compute-redirect-str "/add-node?names=~w&which=~w" names which)))
    ;(compute-param-str "~a?names=~w&which=~w"(referer) names which)))

(define-easy-handler (test :uri "/test") (names)
  (lg :info "test called with referer=~a headers=~a~%" (referer) (hunchentoot:headers-in *request*))
  (generate-standard-page (:title "test")
    (:p "hei")))
  
(define-easy-handler (test2 :uri "/test2") ()
  (generate-standard-page (:title "test")
    (:form :method "post" :action "/test?names=(\"lstel\")"
     (:input :type "submit" :value "knapp"))))



(define-easy-handler (mini-article-not-initiated :uri "/mini-article-not-initiated") (name)
  (lg :debug "Mini-article-not-initiated was called with name: ~a~%" name)
  (if (and (given? name) (ma-exists? name))
    (redirect (serve-mini-article name))
    (generate-standard-page 
      (:title "Where is the mini article?")
      (:strong "Oh, no!")
      (:p "The mini article was not initiated, and not even given as a parameter to this page"))))


(define-easy-handler (build-mini-article :uri "/build-mini-article") ((names :parameter-type #'read-from-string))
  (lg :info "build-mini-article was called with names=~w~%" names)
  (setf names (mapcar #'url-decode names))
  (lg :info "build-mini-article changed names to: ~w~%" names)
  (let ((names (cons (first names) (mapcar #'name-conventions (rest names))))) ; names are already conventionized
    (lg :info "after name-convincing names=~w~%" names)
    (if-returns-are-ok
      (cypher-query
        (format nil "MATCH (problem:Paragraph {name : {problem}}), ~
                     (prob_pic:Picture {name:{prob_pic}}), ~
                     (prob_pic_caption:Paragraph {name:{prob_pic_caption}}), ~
                     (solution:Paragraph {name:{solution}}), ~
                     (sol_pic:Picture {name:{sol_pic}}), ~
                     (sol_pic_caption:Paragraph {name:{sol_pic_caption}}) ~
                     CREATE (new:Mini_article {name:{name}, title:{title}, created:timestamp(), ~
                                              votes:0, uri:{uri}}),
                     (new)-[:PROBLEM]->(problem), ~
                     (new)-[:VISUAL]->(prob_pic), ~
                     (new)-[:CAPTION]->(prob_pic_caption), ~
                     (prob_pic_caption)-[:EXPLAINS]->(prob_pic), ~
                     (prob_pic)-[:EXPLAINS]->(problem), ~
                     (new)-[:SOLUTION]->(solution), ~
                     (new)-[:VISUAL]->(sol_pic), ~
                     (new)-[:CAPTION]->(sol_pic_caption), ~
                     (sol_pic_caption)-[:EXPLAINS]->(sol_pic), ~
                     (sol_pic)-[:EXPLAINS]->(solution) ~
                     RETURN new.name")
                     :params `(("title" . ,(escape-string (first names)))
                               ("name" . ,(name-conventions (first names)))
                               ("problem" . ,(second names));; should apply name-conventions before...
                               ("prob_pic" . ,(third names))
                               ("prob_pic_caption" . ,(fourth names))
                               ("solution" . ,(fifth names))
                               ("sol_pic" . ,(sixth names))
                               ("sol_pic_caption" . ,(seventh names))
                               ("uri" . ,(title->uri (first names))))))
        (progn
          (serve-mini-article (name-conventions (first names)) :serve-pictures? nil)
        (redirect "/print-issue-solution-filter"))))

;; to start it:
;; start neo4j somehow
;; (ql:quickload :pisf)
;; (in-package :pisf)
;; if db is new, run (brand-new-graph.db)
;; (initiate-web-app) 
;; If you're brave: 
;; (insert-all-mini-articles)
;; (serve-all-mini-articles)

;; Next: 
;;    * Make more beautiful vote-buttons on mini-article-pages?
;;    * Make it possible to add mini articles via web interface
;;    * Add ReCaptcha defence against bots
;;    * Server can't handle utf-8! what a shame!
