Print issue solution filter
------------------------

### What is it?
A web app that handles small user-created articles that follow a rigid
template.

### Why make it?
The motivation for writing it was that wiki pages' tables of content (in particular on
the RepRap Wiki) sometimes grow large enough to be hard to navigate and many
enough to be hard to find. 

PISF tries
to solve this by forcing all articles into a small template, and by introducing a tag hierarchy
that navigates (filters) all of them from the same starting point.

Also, I wanted to learn something about web programming, graph databases and
Common Lisp. Before this project I had limited experience with CL and Neo4j,
and none with web-programming.

### How to run it?
 1. Fire up [neo4j](http://www.neo4j.org/) to serve a graph on some adress
 2. Open my-cl-neo4j-2.0.1/config.lisp and feed that adress into the ```*host*``` special variable
 3. Head into pisf-backend/inits.lisp and feed in the path to that directory into the ```*source-pathname*``` special variable
 4. Add your pisf folder and its subfolders to your [quicklisp](http://www.quicklisp.org/beta/) search path. I do this by creating the file ~/.config/common-lisp/source-registry.conf.d/link-farm.conf and adding the line ```(:tree "/path/to/pisf/")``` to it. Check the [ASDF-manual](http://common-lisp.net/project/asdf/asdf.html#Configuring-ASDF-to-find-your-systems) for other methods.
 5. Fire up SBCL and type ```(ql:quickload :pisf)```
 6. Step into the package with ```(in-package :pisf)```
 7. If this is the first time you run pisf, create the bush (category nodes, also called tags) with ```(brand-new-graph.db)```
 8. Now ```(initiate-web-app)```
 9. Point a web browser at [localhost:8080/print-issue-solution-filter](http://localhost:8080/print-issue-solution-filter)


### Features
 * Uses no javascript or cookies
 * It's backend is a Neo4j database and a custom made wrapper to its REST interface, called my-cl-neo4j-2.0.1

### Why such a cumbersome name? 
I don't want to give the impression that this is
high quality code. 

### Based on
hunchentoot, drakma, cl-ppcre and cl-who by Edi Weitz [1](http://weitz.de/),
Lisp For The Web by Adam Petersen [2](http://www.adampetersen.se/articles/lispweb.htm)
SBCL for Linux [3](http://sbcl.org/),
Neo4j by Neo Technology [4](http://www.neo4j.org/)
etc...

### Licence
GPL version 2 or any later version
