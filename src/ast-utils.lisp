;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See COPYING for details.

(in-package :cl-walker)

(defgeneric walk-ast (form visitor)
  (:method :around (form visitor)
    (when (funcall visitor form)
      (call-next-method))
     (values))
  (:method ((form null) visitor)
    ;; nop
    )
  (:method ((form t) visitor)
    ;;(break "~A" form)
    )
  (:method ((form cons) visitor)
    (walk-ast (car form) visitor)
    (walk-ast (cdr form) visitor))
  (:method ((form application-form) visitor)
    (walk-ast (arguments-of form) visitor))
  (:method ((form implicit-progn-mixin) visitor)
    (walk-ast (body-of form) visitor))
  (:method ((form function-binding-form) visitor)
    (walk-ast (bindings-of form) visitor)
    (call-next-method)))

(defun collect-variable-references (top-form)
  (let ((result (list)))
    (walk-ast top-form
              (lambda (form)
                (when (typep form 'variable-reference-form)
                  (push form result))
                t))
    result))
