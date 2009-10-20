;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.walker.documentation)

(def project :hu.dwim.walker :path (system-pathname :hu.dwim.walker))

(def book user-guide (:title "User guide")
  (chapter (:title "Introduction")
    "TODO")
  (chapter (:title "Supported Common Lisp Implementations")
    (paragraph ()
      "SBCL"))
  (chapter (:title "Supported Operating Systems")
    (paragraph ()
      "Linux"))
  (chapter (:title "Tutorial")
    (paragraph ()
      "TODO")))
