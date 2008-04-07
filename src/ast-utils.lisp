;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See COPYING for details.

(in-package :cl-walker)

(defgeneric walk-ast (form visitor)
  (:method-combination progn)
  (:method :around (form visitor)
    (when (funcall visitor form)
      (call-next-method))
     (values))
  (:method progn ((form t) visitor)
    ;; a primary method with a huge NOP
    ))

(macrolet ((frob (&rest entries)
             `(progn
                ,@(loop
                     :for (type . accessors) :in entries
                     :collect `(defmethod walk-ast progn ((form ,type) visitor)
                                 ,@(loop
                                      :for accessor :in accessors
                                      :collect `(walk-ast (,accessor form) visitor)))))))
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
   ;; go-form: dragons be there (and an infinite recursion, too)
   (the-form                  type-of value-of)
   (unwind-protect-form       protected-form-of cleanup-form-of)))

(defun collect-variable-references (top-form &key (type 'variable-reference-form))
  (let ((result (list)))
    (walk-ast top-form
              (lambda (form)
                (when (typep form type)
                  (push form result))
                t))
    result))
