;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.walker.documentation)

(def project :hu.dwim.walker :path (system-pathname :hu.dwim.walker))

(def book user-guide (:title "User guide")
  (chapter (:title "Introduction")
    (paragraph ()
      "A code walker that parses a Common Lisp sexp into a tree of CLOS objects, which is a much more convenient representation to do various code analysis and transformations than sexps. Also features an unwalker that can turn the walked CLOS AST tree back into an sexp.")
    (chapter (:title "Known forks")
      (paragraph ()
        (parse-uri "http://github.com/angavrilov/cl-walker/commits/master/"))))
  (chapter (:title "Supported Common Lisp Implementations")
    (paragraph ()
      "SBCL"))
  (chapter (:title "Tutorial")
    (paragraph ()
      "TODO")))
