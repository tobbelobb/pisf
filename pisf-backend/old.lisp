;; Not used because merge-bush fills its purpose
(defun create-bush ()
  ;; Collect the CREATE statement from *bush-create-pathname* into a string
  ;; and send it as a query to Neo4j...
  (cypher-query (file->string *bush-create-pathname*)))

;; Old control string from create-category-node
            "CREATE (new:Category ~
              {name : {name}, created:timestamp(), accessed:timestamp()}) ~
             WITH new ~
             MATCH (super:Category) ~
             ~@[, (sub:Category)~] ~
             WHERE super.name in {super_names} ~
             ~@[AND sub.name in {sub_names}~] ~
             CREATE (new) -[:IS]-> (super) ~
             ~@[, (sub) -[:IS]-> (new)~] ~
             SET super.accessed=timestamp() ~
             ~@[, sub.accessed=timestamp()~] ~
             RETURN new.name" 


;; Old params for insert-paragraph            
;  ("problem_names" . ,(coerce (listify problem-names) 'simple-vector))
;  ("solution_names" . ,(coerce (listify solution-names) 'simple-vector))
;  ("picture_names" . ,(coerce (listify picture-names) 'simple-vector))
;  ("video_names" . ,(coerce (listify video-names) 'simple-vector))
;  ("figure_names" . ,(coerce (listify caption-of) 'simple-vector)))



;; Old insert-mini-article
;; This function should insert all new nodes.
;;(defun insert-mini-article (title 
;;                            problem-description ; a string to insert into paragraph 
;;                            problem-picture 
;;                            solution-description 
;;                            solution-picture
;;                            is-about ; Point into the bush
;;                            &key
;;                            prob-pic-depicts
;;                            sol-pic-depicts
;;                            big-sol-pic
;;                            big-prob-pic
;;                            sol-fig-name
;;                            prob-fig-name
;;                            solution-name
;;                            problem-name
;;                            prob-fig-caption
;;                            sol-fig-caption
;;                            prob-fig-caption-is-about
;;                            sol-fig-caption-is-about)
;;  (name-conventionize (is-about)
;;    (let* ((prob-pic-name (name-conventions (namestring problem-picture)))
;;           (sol-pic-name (name-conventions (namestring solution-picture)))
;;           (prob-fig-name (aif prob-fig-name (name-conventions it) (name-conventions (first-three-words problem-description))))
;;           (sol-fig-name (aif sol-fig-name (name-conventions it) (name-conventions (first-three-words solution-description))))
;;           (solution-name (aif solution-name (name-conventions it) sol-fig-name))
;;           (problem-name (aif problem-name (name-conventions it) prob-fig-name))
;;           (prob-pic-depicts (aif prob-pic-depicts (name-conventions it) is-about))
;;           (sol-pic-depicts (aif sol-pic-depicts (name-conventions it) is-about))
;;           (commit-uri (extract-commit-uri 
;;                         (insert-picture problem-picture prob-pic-depicts
;;                                         :big-version big-prob-pic
;;                                         :name prob-pic-name
;;                                         :commit? nil))))
;;      (insert-figure prob-fig-name
;;                     :shows prob-pic-name
;;                     :commit-uri commit-uri
;;                     :commit? nil)
;;      (insert-picture solution-picture sol-pic-depicts 
;;                      :big-version big-sol-pic
;;                      :name sol-pic-name
;;                      :commit-uri commit-uri
;;                      :commit? nil)
;;      (insert-figure sol-fig-name
;;                     :shows sol-pic-name
;;                     :commit-uri commit-uri
;;                     :commit? nil)
;;      (insert-solution solution-name 
;;                       :visuals (listify sol-fig-name)
;;                       :commit-uri commit-uri
;;                       :commit? nil)
;;      (insert-problem problem-name
;;                      :visuals (listify prob-fig-name)
;;                      :solved-by solution-name
;;                      :commit-uri commit-uri
;;                      :commit? nil)
;;      (awhen prob-fig-caption 
;;        (insert-paragraph it (aif prob-fig-caption-is-about it is-about)
;;                          :caption-of (listify prob-fig-name)
;;                          :commit-uri commit-uri
;;                          :commit? nil))
;;      (awhen sol-fig-caption 
;;        (insert-paragraph it (aif sol-fig-caption-is-about it is-about)
;;                          :caption-of sol-fig-name
;;                          :commit-uri commit-uri
;;                          :commit? nil))
;;      (insert-paragraph problem-description is-about 
;;                        :describes `(("Problem" . ,problem-name))
;;                        :commit-uri commit-uri
;;                        :commit? nil)
;;      (insert-paragraph solution-description is-about 
;;                        :describes `(("Solution" . ,solution-name))
;;                        :commit-uri commit-uri
;;                        :commit? nil)
;;      (if-elements-with-label-exist-bind-commit-uri 
;;        is-about
;;        "Bush"
;;        commit-uri                                  
;;        (transaction-request
;;          (format nil
;;                  "CREATE (new:Mini_article {name : {name}, ~
;;                   title : {title}, ~
;;                   created : timestamp(), ~
;;                   accessed : timestamp()}) ~
;;                  WITH new ~
;;                  FOREACH (bush_name in {bush_names} | ~
;;                                     MERGE (b:Bush {name:bush_name}) ~
;;                                     CREATE (new)-[:IS_ABOUT]->(b) ~
;;                                     SET b.accessed = timestamp()) ~
;;                  WITH new ~
;;                  MATCH (p:Problem {name : {problem_name}}), ~
;;                  (s:Solution {name : {solution_name}}) ~
;;                  CREATE ~
;;                  (s)<-[:SOLUTION]-(new)-[:PROBLEM]->(p)")
;;                  `(("name" . ,(name-conventions title))
;;                    ("title" . ,title)
;;                    ("problem_name" . ,problem-name)
;;                    ("solution_name" . ,solution-name)
;;                    ("bush_names" . ,(coerce (listify is-about) 'simple-vector)))
;;                  :commit-uri commit-uri :commit? t)))))


(defun insert-figure (name &key
                           shows ; picture node name or list of picture node names
                           visual-of ; Problem or Solution names
                           caption-paragraph
                           (commit-uri nil)
                           (commit? t)) ; name of paragraph node
  (name-conventionize (name shows caption-paragraph visual-of)
    (if-elements-with-label-exist-bind-commit-uri
      shows ; accepts empty list
      "Picture"
      commit-uri
      (if-elements-with-label-exist-bind-commit-uri 
        caption-paragraph ; accepts empty list
        "Paragraph"
        commit-uri
        (if-elements-with-label-exist-bind-commit-uri 
          visual-of ; accepts empty list
          "Describable" ; Problems and solutions are describable
          commit-uri
          (transaction-request
            (format nil
                    "CREATE ~
                     (new:Figure:Visual {name : {name}, created : timestamp(), accessed : timestamp()}) ~
                     WITH new ~
                     ~:[~;FOREACH (picture_name in {picture_names} | ~
                                        MERGE (p:Picture {name:picture_name}) ~
                                        CREATE (new)-[:SHOWS]->(p) ~
                                        SET p.accessed = timestamp())~] ~
                     ~:[~;FOREACH (visualized_name in {visualized_names} | ~
                                        MERGE (d:Describable {name:visualized_name}) ~
                                        CREATE (new)<-[:VISUAL]-(d) ~
                                        SET d.accessed = timestamp())~] ~
                     ~:[~;MERGE (c:Paragraph {name:{paragraph_name}}) ~
                                ON MATCH SET c.accessed=timestamp() ~
                                CREATE (new)-[:CAPTION]->(c)~] ~
                     "
                    shows visual-of caption-paragraph)
          (append `(("name" . ,name))
                    (loop for l in (list shows visual-of caption-paragraph)
                          and param-name in '("picture_names" "visualized_names" "paragraph_name")
                          when l
                          collect `(,param-name . ,(if (string= "paragraph_name" param-name) 
                                                     l 
                                                     (coerce (listify l) 'simple-vector)))))
          :commit-uri commit-uri :commit? commit?))))))

(defun insert-problem (name &key
                            visuals
                            description
                            mini-articles
                            solved-by
                            (commit-uri nil)
                            (commit? t))
  (insert-prob-or-sol name :visuals visuals
                           :description description
                           :mini-articles mini-articles
                           :solved-by solved-by
                           :prob-or-sol "Problem"
                           :commit-uri commit-uri
                           :commit? commit?))

(defun insert-solution (name &key
                             visuals
                             description
                             mini-articles
                             solves
                             (commit-uri nil)
                             (commit? t))
  (insert-prob-or-sol name :visuals visuals
                           :description description
                           :mini-articles mini-articles
                           :solves solves
                           :prob-or-sol "Solution"
                           :commit-uri commit-uri
                           :commit? commit?))

(defun insert-prob-or-sol (name &key
                             visuals
                             description
                             mini-articles
                             solves
                             solved-by
                             (prob-or-sol "Solution")
                             (commit-uri nil)
                             (commit? t))
  (if (or (string= prob-or-sol "Solution") (string= prob-or-sol "Problem"))
    (name-conventionize (name visuals description mini-articles solves solved-by)
      (if-elements-with-label-exist-bind-commit-uri
        visuals ; accepts empty list
        "Visual"
        commit-uri
        (if-elements-with-label-exist-bind-commit-uri 
          description; accepts empty list
          "Paragraph"
          commit-uri
          (if-elements-with-label-exist-bind-commit-uri 
            mini-articles; accepts empty list
            "Mini_article" ; Problems and solutions are describable
            commit-uri
            (if-elements-with-label-exist-bind-commit-uri 
              solved-by ; accepts empty list
              "Solution" ; Problems and solutions are describable
              commit-uri
              (if-elements-with-label-exist-bind-commit-uri 
                solves ; accepts empty list
                "Problem" ; Problems and solutions are describable
                commit-uri
                (transaction-request
                  (format nil
                          "CREATE ~
                           (new:~a:Describable {name : {name}, created : timestamp(), accessed : timestamp()}) ~
                           WITH new ~
                           ~:[~;FOREACH (visual_name in {visual_names} | ~
                                              MERGE (v:Visual {name:visual_name}) ~
                                              CREATE (new)-[:VISUAL]->(v) ~
                                              SET v.accessed = timestamp())~] ~
                           ~:[~;MERGE (p:Paragraph {name:{paragraph_name}}) ~
                                      ON MATCH SET p.accessed=timestamp() ~
                                      CREATE (new)<-[:DESCRIBES]-(p)~] ~
                           ~:[~;FOREACH (problem_name in {problem_names} | ~
                                              MERGE (prob:Problem {name:problem_name}) ~
                                              CREATE (new)-[:SOLVES]->(prob) ~
                                              SET prob.accessed = timestamp())~] ~
                           ~:[~;FOREACH (solution_name in {solution_names} | ~
                                              MERGE (sol:Solution {name:solution_name}) ~
                                              CREATE (new)<-[:SOLVES]-(sol) ~
                                              SET sol.accessed = timestamp())~] ~
                           ~:[~;FOREACH (mini_article_name in {mini_article_names} | ~
                                              MERGE (m:Mini_article {name:mini_article_name}) ~
                                              CREATE (new)<-[:~a]-(m) ~
                                              SET m.accessed = timestamp())~]"
                          prob-or-sol visuals description solves solved-by mini-articles 
                          (string-upcase prob-or-sol))
                (append `(("name" . ,name))
                          (loop for l in (list visuals description mini-articles solves solved-by)
                                and param-name in '("visual_names" 
                                                    "paragraph_name" 
                                                    "mini_article_names" 
                                                    "problem_names"
                                                    "solution_names")
                                when l
                                collect `(,param-name . ,(if (string= "paragraph_name" param-name) 
                                                           l 
                                                           (coerce (listify l) 'simple-vector)))))
                :commit-uri commit-uri :commit? commit?)))))))
  (lg :warn "Tried to use insert-prob-or-sol with label ~s. Aborted." prob-or-sol)))

(defun name-conventions (n)
  (do* ((line (string-downcase (string-upcase (string-trim " " n) :end 1) :start 1)
              (string-upcase line :start (+ space-pos 1) :end (+ space-pos 2)))
        (space-pos (position #\Space line) 
                   (position #\Space line :start (+ space-pos 1))))
    ((null space-pos) line)))
