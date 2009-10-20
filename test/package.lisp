;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :common-lisp-user)

(defpackage :hu.dwim.walker.test
  (:use :hu.dwim.common
        :hu.dwim.stefil
        :hu.dwim.walker))

(in-package :hu.dwim.walker.test)

(defsuite* (test :in root-suite))
