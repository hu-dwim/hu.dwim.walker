;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See COPYING for details.

(in-package :cl-walker)

(defgeneric map-ast (visitor form)
  (:method-combination progn)
  (:method :around (visitor form)
    (let ((new (funcall visitor form)))
      (if (eq new form)
          (call-next-method)
          new)
      new))
  (:method progn (visitor (form t))
    ;; a primary method with a huge NOP
    ))

(macrolet ((frob (&rest entries)
             `(progn
                ,@(loop
                     :for (type . accessors) :in entries
                     :collect `(defmethod map-ast progn (visitor (form ,type))
                                 ,@(loop
                                      :for accessor :in accessors
                                      :collect `(map-ast visitor (,accessor form))))))))
  (frob
   (cons                      car cdr)
   (application-form          operator-of arguments-of)
   (lambda-function-form      arguments-of)
   (optional-function-argument-form default-value-of)
   (keyword-function-argument-form default-value-of)
   (implicit-progn-mixin      body-of)
   (binding-form-mixin        bindings-of)

   (return-from-form result-of)
   (throw-form                value-of)
   (if-form                   condition-of then-of else-of)
   (multiple-value-call-form  arguments-of function-designator-of)
   (multiple-value-prog1-form first-form-of other-forms-of)
   (progv-form                variables-form-of values-form-of)
   (setq-form                 variable-of value-of)
   ;; go-form: leave it alone, dragons be there (and an infinite recursion, too)
   (the-form                  type-of value-of)
   (unwind-protect-form       protected-form-of cleanup-form-of)))

(defun collect-variable-references (top-form &key (type 'variable-reference-form))
  (let ((result (list)))
    (map-ast (lambda (form)
               (when (typep form type)
                 (push form result))
               form)
             top-form)
    result))
