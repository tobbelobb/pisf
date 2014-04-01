Print issue solution filter is a web app that handles small user-created articles with exactly two paragraphs and two pictures each.
The motivation for writing it was that wiki pages' tables of content (in particular on
the RepRap Wiki) sometimes grow large enough to be hard to navigate and many
enough to be hard to find. 

PISF tries
to solve this by forcing all articles into a small template, and by introducing a tag hierarchy
that navigates (filters) all of them from the same starting point.

It's backend is a Neo4j database and a custom made wrapper to its REST
interface, called my-cl-neo4j-2.0.1.

Why such a cumbersome name? I don't want to give the impression that this is
high quality code. The other great motivation for why I wrote
this web app was that I wanted to learn something about web programming, graph databases and
Common Lisp.
