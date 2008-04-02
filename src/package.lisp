;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See COPYING for details.

(cl:in-package :common-lisp-user)

(defpackage :cl-walker
  (:documentation "A code walker for Common Lisp")

  (:use :common-lisp :alexandria)

  (:shadow
   #:type-of)

  (:export

   ;;;
   ;;; environment
   ;;;
   #:make-empty-lexenv
   #:lookup-in-lexenv
   #:macroexpand-all

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

   ;;;
   ;;; AST utils
   ;;;
   #:collect-variable-references

   ;;;
   ;;; walker
   ;;;
   #:form
   #:walk-form
   #:unwalk-form
   #:unwalk-forms
   #:unwalk-lambda-list
   #:macroexpand-all
   #:make-walkenv
   #:*warn-undefined*

   #:undefined-reference
   #:undefined-variable-reference
   #:undefined-function-reference
   #:return-from-unknown-block

   #:defwalker-handler
   #:defunwalker-handler

   #:implicit-progn-mixin
   #:implicit-progn-with-declare-mixin
   #:binding-form-mixin
   #:declaration-form
   #:constant-form
   #:variable-reference-form
   #:lexical-variable-reference-form
   #:unwalked-lexical-variable-reference-form
   #:free-variable-reference-form
   #:application-form
   #:lexical-application-form
   #:walked-lexical-application-form
   #:unwalked-lexical-application-form
   #:free-application-form
   #:lambda-application-form
   #:function-form
   #:lambda-function-form
   #:function-object-form
   #:walked-lexical-function-object-form
   #:free-function-object-form
   #:unwalked-lexical-function-object-form
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

   #:walk-lambda-list
   #:walk-implict-progn
   #:body
   #:cleanup-form
   #:code-of
   #:consequent
   #:declares
   #:default-value
;; #:else ; iterate
   #:enclosing-tagbody-of
   #:eval-when-times
   #:first-form
   #:func
   #:keyword-name
   #:name-of
   #:other-forms
   #:parent
   #:protected-form
   #:read-only-p
   #:result
   #:source
   #:specializer-of
   #:supplied-p-parameter
   #:tag
   #:target-block-of
   #:jump-target-of
   #:then
   ;;#:type-of
   #:value-of
   #:values-form
   #:variable-name-of
   #:vars-form

   ))
