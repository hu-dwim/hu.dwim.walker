;; -*- lisp -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :it.bese.arnesi.system)
    (defpackage :it.bese.arnesi.system
      (:documentation "ASDF System package for ARNESI.")
      (:use :common-lisp :asdf))))

(in-package :it.bese.arnesi.system)

(defsystem :arnesi
  :components ((:static-file "arnesi.asd")
               (:module :src
                :components ((:file "accumulation" :depends-on ("packages" "one-liners"))
                             (:file "asdf" :depends-on ("packages" "io"))
                             (:file "csv" :depends-on ("packages" "string"))
                             (:file "compat" :depends-on ("packages"))
                             (:module :call-cc
                              :components ((:file "interpreter")
                                           (:file "handlers")
                                           (:file "apply")
                                           (:file "generic-functions")
                                           (:file "common-lisp-cc"))
                              :serial t
                              :depends-on ("packages" "walk" "flow-control" "list" "string" "defclass-struct"))
			     (:file "debug" :depends-on ("accumulation"))
                             (:file "decimal-arithmetic" :depends-on ("packages"))
                             (:file "defclass-struct" :depends-on ("packages" "list"))
                             (:file "flow-control" :depends-on ("packages" "one-liners"))
                             (:file "hash" :depends-on ("packages" "list" "one-liners" "string"))
                             (:file "http" :depends-on ("packages" "vector" "string"))
                             (:file "io" :depends-on ("packages" "flow-control"))
                             (:file "lambda" :depends-on ("packages"))
                             (:file "lexenv" :depends-on ("packages"))
                             (:file "list" :depends-on ("packages" "one-liners" "accumulation" "flow-control"))
                             (:file "log" :depends-on ("packages" "numbers" "hash" "io"))
                             (:file "matcher" :depends-on ("packages" "hash" "list" "flow-control" "one-liners"))
                             (:file "mop" :depends-on ("packages" "mopp"))
			     (:file "mopp" :depends-on ("packages" "list" "flow-control"))
                             (:file "numbers" :depends-on ("packages"))
                             (:file "one-liners" :depends-on ("packages"))
                             (:file "packages")
                             (:file "queue" :depends-on ("packages"))
                             (:file "sequence" :depends-on ("packages"))
                             (:file "read-macros" :depends-on ("list"))
                             (:file "sharpl-reader" :depends-on ("packages" "flow-control"))
                             (:file "specials" :depends-on ("packages" "hash"))
                             (:file "string" :depends-on ("packages" "list"))
                             (:file "time" :depends-on ("packages"))
			     (:file "unwalk" :depends-on ("packages" "walk"))
                             (:file "vector" :depends-on ("packages" "flow-control"))
                             (:file "walk" :depends-on ("packages" "list" "mopp" "lexenv"
                                                        "one-liners")))))
  :properties ((:features "v1.4.0" "v1.4.1" "v1.4.2" "cc-interpreter"
                          "join-strings-return-value")))

(defsystem :arnesi.test
  :components ((:module :t
		:components ((:file "accumulation" :depends-on ("suite"))
                             (:file "call-cc" :depends-on ("suite"))
                             (:file "http" :depends-on ("suite"))
                             (:file "log" :depends-on ("suite"))
                             (:file "matcher" :depends-on ("suite"))
                             (:file "numbers" :depends-on ("suite"))
                             (:file "queue" :depends-on ("suite"))
                             (:file "read-macros" :depends-on ("suite"))
                             (:file "string"  :depends-on ("suite"))
                             (:file "sequence" :depends-on ("suite"))
			     (:file "walk" :depends-on ("suite"))
                             (:file "suite"))))
  :depends-on (:arnesi :FiveAM)
  :in-order-to ((compile-op (load-op :arnesi))))

(defsystem :arnesi.cl-ppcre-extras
  :components ((:module :src
                :components ((:file "cl-ppcre-extras"))))
  :depends-on (:cl-ppcre :arnesi))

(defmethod perform ((op asdf:test-op) (system (eql (find-system :arnesi))))
  (asdf:oos 'asdf:load-op :arnesi.test)
  (funcall (intern (string :run!) (string :it.bese.FiveAM))
           :it.bese.arnesi))

;;;; * Introduction

;;;; A collection of various common lisp utilites.

;;;;@include "src/packages.lisp"
