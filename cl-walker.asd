;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See COPYING for details.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package '#:cl-walker.system)
    (defpackage #:cl-walker.system
      (:use :common-lisp :asdf))))

(cl:in-package :cl-user)

(in-package cl-walker.system)

(defsystem :cl-walker
  :depends-on (:alexandria)
  :components ((:static-file "cl-walker.asd")
               (:module "src"
                :components ((:file "package")
                             (:file "duplicates" :depends-on ("package"))
                             #+sbcl(:file "lexenv-sbcl" :depends-on ("duplicates" "package"))
                             (:file "lexenv-late" :depends-on ("package" "duplicates"
                                                               #+sbcl "lexenv-sbcl"))
                             (:file "infrastructure" :depends-on ("package" "lexenv-late" "duplicates"))
                             (:file "walk" :depends-on ("infrastructure"))
                             (:file "implementation-specific" :depends-on ("infrastructure"))
                             (:file "unwalk" :depends-on ("infrastructure"))))))

(defsystem :cl-walker-test
  :components ((:module "tests"
                :components ((:file "package")
                             (:file "walk-unwalk" :depends-on ("package"))
                             (:file "macros" :depends-on ("package")))))
  :depends-on (:cl-walker :stefil))

(defmethod perform ((op asdf:test-op) (system (eql (find-system :cl-walker))))
  (asdf:oos 'asdf:load-op :cl-walker-test)
  (in-package :cl-walker-test)
  (declaim (optimize (debug 3)))
  (warn "(declaim (optimize (debug 3))) was issued to help later C-c C-c'ing")
  (eval (read-from-string "(progn
                             (stefil:funcall-test-with-feedback-message 'test))"))
  (values))

(defmethod operation-done-p ((op test-op) (system (eql (find-system :cl-walker))))
  nil)
