;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.walker.test
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.test-system"
  :depends-on (:hu.dwim.stefil+hu.dwim.def
               :hu.dwim.stefil+swank
               :hu.dwim.util.temporary-files
               :hu.dwim.walker)
  :components ((:module "test"
                :components ((:file "ast" :depends-on ("package"))
                             (:file "lexenv" :depends-on ("package"))
                             (:file "macros" :depends-on ("package" "walk-unwalk"))
                             (:file "package")
                             (:file "walk-unwalk" :depends-on ("package" "lexenv"))
                             (:file "semantics" :depends-on ("package" "lexenv"))))))
