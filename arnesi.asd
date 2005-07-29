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
                             (:static-file "arnesi.el")
                             (:file "asdf" :depends-on ("packages" "io"))
                             (:file "csv" :depends-on ("packages" "string"))
                             (:file "compat" :depends-on ("packages"))
                             (:file "cps" :depends-on ("packages" "walk" "flow-control" "list" "string" "defclass-struct"))
			     (:file "debug" :depends-on ("accumulation"))
                             (:file "decimal-arithmetic" :depends-on ("packages"))
                             (:file "defclass-struct" :depends-on ("packages" "list"))
                             (:file "flow-control" :depends-on ("packages" "one-liners"))
                             (:file "hash" :depends-on ("packages" "list" "one-liners" "string"))
                             (:file "http" :depends-on ("packages" "vector"))
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
                             (:file "sequence" :depends-on ("packages"))
                             (:file "sharpl-reader" :depends-on ("packages" "flow-control"))
                             (:file "specials" :depends-on ("packages" "hash"))
                             (:file "string" :depends-on ("packages" "list"))
                             (:file "vector" :depends-on ("packages" "flow-control"))
                             (:file "walk" :depends-on ("packages" "list" "mopp" "lexenv")))))
  :properties ((:features "v1.4.0" "v1.4.1" "cps-interpreter")))

(defsystem :arnesi.test
  :components ((:module :t
		:components ((:file "accumulation" :depends-on ("suite"))
                             (:file "cps" :depends-on ("suite"))
                             (:file "log" :depends-on ("suite"))
                             (:file "matcher" :depends-on ("suite"))
                             (:file "numbers" :depends-on ("suite"))
                             (:file "string"  :depends-on ("suite"))
                             (:file "sequence" :depends-on ("suite"))
                             (:file "suite"))))
  :depends-on (:arnesi :FiveAM)
  :in-order-to ((compile-op (load-op :arnesi))))

(defsystem :arnesi.cl-ppcre-extras
  :components ((:module :src
                :components ((:file "cl-ppcre-extras"))))
  :depends-on (:cl-ppcre :arnesi))

(defmethod perform ((op asdf:test-op) (system (eql (find-system :arnesi))))
  (asdf:oos 'asdf:load-op :arnesi.test)
  (funcall (intern (string :run!) (string :it.bese.5am))
           :it.bese.arnesi))

;;;;@include "src/packages.lisp"
