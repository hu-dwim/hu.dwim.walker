;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See COPYING for details.

(cl:in-package :cl-user)

(defpackage :cl-walker-test
  (:use :common-lisp
        :cl-walker
        :stefil))

(in-package :cl-walker-test)

(in-root-suite)

(defsuite* test)
