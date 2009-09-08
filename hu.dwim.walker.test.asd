;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.walker.test
  :class hu.dwim.test-system
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
           "Levente Mészáros <levente.meszaros@gmail.com>")
  :licence "BSD / Public domain"
  :description "Test suite for hu.dwim.walker"
  :depends-on (:hu.dwim.stefil
               :hu.dwim.walker)
  :components ((:module "test"
                :components ((:file "ast" :depends-on ("package"))
                             (:file "lexenv" :depends-on ("package"))
                             (:file "macros" :depends-on ("package"))
                             (:file "package")
                             (:file "walk-unwalk" :depends-on ("package"))))))
