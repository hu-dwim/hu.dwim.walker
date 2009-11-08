;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :common-lisp-user)

(defpackage :hu.dwim.walker
  (:documentation "A code walker for Common Lisp")

  (:use :alexandria
        :anaphora
        :contextl
        :hu.dwim.common-lisp
        :hu.dwim.def
        :hu.dwim.defclass-star
        :hu.dwim.util
        :metabang-bind)

  (:shadow #:type-of
           #:eval)

  (:export ;; environment (intentionally kept here instead of definer export, because they need to be implemented on some platforms)
           #:make-empty-lexical-environment
           #:lookup-in-lexenv
           #:macroexpand-all

           #:-environment-
           #:-form-
           #:-parent-

           #:do-variables-in-lexenv
           #:do-functions-in-lexenv
           #:do-macros-in-lexenv
           #:do-symbol-macros-in-lexenv
           #:do-blocks-in-lexenv
           #:do-tags-in-lexenv

           #:iterate-variables-in-lexenv
           #:iterate-functions-in-lexenv
           #:iterate-macros-in-lexenv
           #:iterate-symbol-macros-in-lexenv
           #:iterate-blocks-in-lexenv
           #:iterate-tags-in-lexenv

           #:collect-variables-in-lexenv
           #:collect-functions-in-lexenv
           #:collect-macros-in-lexenv
           #:collect-symbol-macros-in-lexenv
           #:collect-blocks-in-lexenv
           #:collect-tags-in-lexenv

           #:find-variable-in-lexenv
           #:find-function-in-lexenv
           #:find-macro-in-lexenv
           #:find-symbol-macro-in-lexenv
           #:find-block-in-lexenv
           #:find-tag-in-lexenv

           ;; some utils
           #:collect-variable-references
           #:special-variable-name?
           #:compute-binding-usages

           ;; conditions
           #:walker-error
           #:simple-walker-error
           #:return-from-unknown-block
           #:illegal-lambda-list

           ;; walker
           #:map-ast
           #:make-walk-environment

           #:with-form-object
           #:walk-implict-progn

           #:body-of
           #:cleanup-form-of
           #:code-of
           #:condition-of
           #:default-value-of
           #:else-of
           #:enclosing-tagbody-of
           #:eval-when-times
           #:first-form-of
           #:function-designator-of
           #:keyword-name-of
           #:effective-keyword-name-of
           #:name-of
           #:other-forms-of
           #:parent-of
           #:protected-form-of
           #:result-of
           #:source-of
           #:specializer-of
           #:supplied-p-parameter
           #:tag-of
           #:target-block-of
           #:jump-target-of
           #:then-of
           #:usages-of
           ;; #:type-of
           #:value-of
           #:values-form-of
           #:variable-of
           #:variables-form-of
           #:operator-of
           #:arguments-of

           #:collect-standard-walked-form-subclasses))
