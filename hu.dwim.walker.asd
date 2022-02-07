;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem "hu.dwim.walker"
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :author ("Attila Lendvai <attila@lendvai.name>"
           "Levente Mészáros <levente.meszaros@gmail.com>")
  :description "Common Lisp form walker and unwalker (to and from CLOS instances)."
  :depends-on (:alexandria
               :anaphora
               :contextl
               :closer-mop
               :hu.dwim.def+contextl
               :hu.dwim.defclass-star+hu.dwim.def
               :hu.dwim.util
               :metabang-bind)
  :components ((:file "package" :pathname "source/package")
               (:module "integration"
                :depends-on ("package")
                :components (#+allegro(:file "allegro")
                             #+clisp(:file "clisp")
                             #+cmu(:file "cmucl")
                             #+lispworks(:file "lispworks")
                             #+openmcl(:file "openmcl")
                             #+ecl(:file "ecl")
                             #+sbcl(:file "sbcl")))
               (:module "source"
                :depends-on ("integration" "package")
                :components ((:file "api" :depends-on ("conditions" "duplicates"))
                             (:file "ast" :depends-on ("infrastructure" "handler" "progn" "function"))
                             (:file "conditions" :depends-on ("variables"))
                             (:file "duplicates")
                             (:file "function" :depends-on ("infrastructure" "progn"))
                             (:file "handler" :depends-on ("infrastructure" "function"))
                             (:file "infrastructure" :depends-on ("api" "lexenv"))
                             (:file "lexenv" :depends-on ("api"))
                             (:file "macro" :depends-on ("function" "infrastructure" "progn"))
                             (:file "progn" :depends-on ("infrastructure"))
                             (:file "variables")))))

(defsystem "hu.dwim.walker/test"
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.test-system"
  :depends-on (:hu.dwim.stefil+hu.dwim.def
               :hu.dwim.stefil+swank
               :hu.dwim.util/temporary-files
               :hu.dwim.walker)
  :components ((:module "test"
                :components ((:file "ast" :depends-on ("package"))
                             (:file "lexenv" :depends-on ("package"))
                             (:file "macros" :depends-on ("package" "walk-unwalk"))
                             (:file "package")
                             (:file "walk-unwalk" :depends-on ("package" "lexenv"))
                             (:file "semantics" :depends-on ("package" "lexenv"))))))

(defsystem :hu.dwim.walker/documentation
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.documentation-system"
  :depends-on (:hu.dwim.walker/test
               :hu.dwim.presentation)
  :components ((:module "documentation"
                :components ((:file "package")
                             (:file "walker" :depends-on ("package"))))))
