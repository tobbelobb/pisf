(in-package :pisf)

(defmacro generate-standard-page ((&key title script) &body body)
  `(with-html-output-to-string
       (*standard-output* nil :prologue t :indent nil)
     (:html :lang "en"
	    (:head
	     (:meta :charset "utf-8")
	     (:title ,title)
	     (:link :type "text/css"
		    :rel "stylesheet"
		    :href "style.css"))
	    ,(when script ; User of macro has provided javascript that we should make inline
		   `(:script :type "text/javascript"
			     (str ,script)))
	    (:body
	     (:div :id "header"
		   (:a :href "http://reprap.org/wiki/Main_Page" 
		       (:img :src "/pictures/teardrop.jpg"
			     :alt "RepRap Teardrop"
			 :class "logo")))
	     (:div :id "SiteName" 
		   (:a :href "/print-issue-solution-filter"
		       "The RepRap Print Issue Solution Filter"))
	     (:div :id "MainContent"
		   ,@body)))))

;; These heve defun-versions in pisf.lisp
(defmacro submit (&key (value "Submit"))
  `(htm
     (:p (:input :type "submit" :value ,value :class "btn"))
     (values)))


;; What happens if user Tries tags that doesn't exist?
(defmacro tags ()
  `(htm
     (:p "Tags" (:br)
      (bind-cypher-returns-and-loop ("MATCH (b:Bush) WITH b.name AS name ORDER BY name RETURN name")
        (htm
          (:input :type "checkbox" :name "is-about" :value name (str name))(:br))))
     (values)))

(defmacro add-new-tag-form ()
  `(htm
     (:strong "Can't find a tag? Create it here:")
     (:form :action (compute-link-str "/tag-added?names=~w&which=~w" names which) :method "post" :id "addform" :label "New tag"
      (:p "Name" (:br)
       (:input :type "text" :name "name" :class "txt" :required t :size 25 :label "Name (required)"))
      (:p "Supertags" (:br)
       (:input :type "text" :name "super-categories" :class "txt" :required t
        :pattern (str *csv-regex*)
        :placeholder "Tag1, Tag2, ..." :title *csv-regex-explanation*
        :label "Supertags (required)")) 
      (:p "Subtags" (:br)
       (:input :type "text" :name "sub-categories" :class "txt"
        :pattern (str *csv-regex*)
        :placeholder "Tag1, Tag2, ..." :title *csv-regex-explanation*
        :label "Subtags")) 
      (:p (:input :type "submit" :value "Submit tag" :class "btn")))))

(defmacro name ()
  `(htm (:p "Name" (:br)
         (:input :type "text" :name "new-name" :placeholder "Name" :class "txt"))
        (values)))

(defmacro content-form (&key (it-str nil))
  `(htm
     (:form :action (compute-link-str "/receive-paragraph?names=~w&which=~w" names which) :method "post"
      (:p (:textarea :cols 62 :rows 6 :name "content" :placeholder "Content" :required t :autofocus t :class "txt") (:br))
      (:p "Name paragraph (optional)" (:br)
       (:input :type "text" :name "new-name" :placeholder "Name" :class "txt"))
      (tags)
      (submit :value ,(if it-str `(format nil "Submit ~a" ,it-str) "Submit")))
     (:div :class "bottom" (add-new-tag-form))
     (values)))

(defmacro picture-form (&key it-str)
  `(htm
     (:form :action (compute-link-str "/receive-picture?names=~w&which=~w" names which) :method "post" :enctype "multipart/form-data"
      (:p (:input :type "file" :accept "image/*" :name "path-data" :required t :autofocus t))
      (tags)
      (submit :value ,(if it-str `(format nil "Submit ~a" ,it-str) "Submit")))
     (:div :class "bottom" (add-new-tag-form))
     (values)))

(defmacro title-form ()
  `(htm
     (:form :action (compute-link-str "/receive-title?names=~w" names) :method "post" 
      (:p "Title" (:br)
       (:input :type "text" :name "new-name" :class "txt" :required t :size 25
        :placeholder "Title" :autofocus t))
      (:div :class "bottom" (submit :value "Submit title")))
     (values)))

;; cypher-queries, create-bush-node (and everything that uses neo-request?)
;; may be wrapped in this
;; wrap everything you want to do after database-interaction in a progn (no
;; implicit)...
(defmacro if-returns-are-ok (&body body)
  (if (> (length body) 1)
  `(multiple-value-bind (ret-val return-code reason-string) ,(first body)
     (lg :error "if-returns-are-ok got code ~d~%" return-code)
     (case return-code
       (999
        (generate-standard-page (:title "Oh, no!")
          (:h1 "Oops...") 
          (:p (fmt "PISF is really sorry, it can't find node(s) named ~
                    ~{~a~^,~} with label ~s in the database. ~
                    A node insertion was probably aborted." (first ret-val) (third ret-val)))))
       (400 
        (generate-standard-page (:title "Oh, no!")
          (:h1 "Oops...") 
          (:p (fmt "PISF is really sorry, but Neo4j says ~s and ~s. ~
                    Probably a query contains an error." reason-string 
                   (ca :message ret-val)))))
       (t
         (if-returns-are-ok ,@(rest body)))))
    (first body)))

