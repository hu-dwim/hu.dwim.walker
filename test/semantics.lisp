;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.walker.test)

(defsuite* (test/semantics :in test))

(def test test/semantics/flet ()
  (bind ((walked (walk-form '(flet ((foo ()
                                     1))
                              (flet ((foo ()
                                       2))
                                (funcall #'foo)))))
         (function-object-form (finishes (first (arguments-of (first (body-of (first (body-of walked)))))))))
    (is (typep function-object-form 'walked-lexical-function-object-form))
    (bind ((walked-lexical-definition (definition-of function-object-form)))
      (is (typep walked-lexical-definition 'lambda-function-form))
      (bind ((binding (find walked-lexical-definition (bindings-of (parent-of walked-lexical-definition)) :key #'cdr)))
        (is (not (null binding)))
        (is (eq (first binding) 'foo))
        ;; check if we looked up the innermost 'foo
        (is (eql 2 (value-of (first (body-of walked-lexical-definition)))))))))
