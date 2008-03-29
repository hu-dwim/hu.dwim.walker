;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See COPYING for details.

(cl:in-package :common-lisp-user)

(defpackage :cl-walker
  (:documentation "A code walker for Common Lisp")
  (:use :common-lisp :alexandria)
  (:export

   #:make-empty-lexenv
   #:macroexpand-all

   #:iterate-variables-in-lexenv
   #:iterate-functions-in-lexenv
   #:iterate-macros-in-lexenv
   #:iterate-symbol-macros-in-lexenv

   #:collect-variables-in-lexenv
   #:collect-functions-in-lexenv
   #:collect-macros-in-lexenv
   #:collect-symbol-macros-in-lexenv

   #:find-variable-in-lexenv
   #:find-function-in-lexenv
   #:find-macro-in-lexenv
   #:find-symbol-macro-in-lexenv

   #:form
   #:walk-form
   #:make-walk-environment
   #:*walk-handlers*
   #:*warn-undefined*
   #:undefined-reference
   #:undefined-variable-reference
   #:undefined-function-reference
   #:return-from-unknown-block
   #:defwalker-handler
   #:implicit-progn-mixin
   #:implicit-progn-with-declare-mixin
   #:binding-form-mixin
   #:declaration-form
   #:constant-form
   #:variable-reference
   #:local-variable-reference
   #:local-lexical-variable-reference
   #:free-variable-reference
   #:application-form
   #:local-application-form
   #:lexical-application-form
   #:free-application-form
   #:lambda-application-form
   #:function-form
   #:lambda-function-form
   #:function-object-form
   #:local-function-object-form
   #:free-function-object-form
   #:lexical-function-object-form
   #:function-argument-form
   #:required-function-argument-form
   #:specialized-function-argument-form
   #:optional-function-argument-form
   #:keyword-function-argument-form
   #:allow-other-keys-function-argument-form
   #:rest-function-argument-form
   #:block-form
   #:return-from-form
   #:catch-form
   #:throw-form
   #:eval-when-form
   #:if-form
   #:function-binding-form
   #:flet-form
   #:labels-form
   #:variable-binding-form
   #:let-form
   #:let*-form
   #:locally-form
   #:macrolet-form
   #:multiple-value-call-form
   #:multiple-value-prog1-form
   #:progn-form
   #:progv-form
   #:setq-form
   #:symbol-macrolet-form
   #:tagbody-form
   #:go-tag-form
   #:go-form
   #:the-form
   #:unwind-protect-form
   #:extract-argument-names
   #:walk-lambda-list
   #:walk-implict-progn
   #:arguments
   #:binds
   #:body
   #:cleanup-form
   #:code
   #:consequent
   #:declares
   #:default-value
;; #:else ; iterate
   #:enclosing-tagbody
   #:eval-when-times
   #:first-form
   #:func
   #:keyword-name
   #:name
   #:operator
   #:optimize-spec
   #:other-forms
   #:parent
   #:protected-form
   #:read-only-p
   #:result
   #:source
;; #:specializer ; closer-mop
   #:supplied-p-parameter
   #:tag
   #:target-block
   #:target-progn
   #:then
   #:type-form
   #:value
   #:values-form
   #:variable-name-of
   #:vars-form

   #:defunwalker-handler
   #:unwalk-form
   #:unwalk-forms
   #:unwalk-lambda-list
   ))
