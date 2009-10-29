;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.walker.test)

(defsuite* (test/semantics :in test))

(def test test/semantics/flet/1 ()
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

(def test test/semantics/flet/2 ()
  (bind ((walked (walk-form '(flet ((outer ()
                                     42))
                              (flet ((inner ()
                                       (outer)))
                                (inner)))))
         ;; fetch the (inner) call
         (application-form/inner (finishes (first (body-of (first (body-of walked)))))))
    (is (typep application-form/inner 'walked-lexical-application-form))
    (bind ((walked-lexical-definition (definition-of application-form/inner)))
      (is (typep walked-lexical-definition 'lambda-function-form))
      ;; lookup the binding for #'inner
      (bind ((binding (find walked-lexical-definition (bindings-of (parent-of walked-lexical-definition)) :key #'cdr)))
        (is (not (null binding)))
        (is (eq (first binding) 'inner))
        ;; fetch the (outer) call inside #'inner
        (bind ((application-form/outer (first (body-of walked-lexical-definition))))
          (is (typep application-form/outer 'walked-lexical-application-form))
          (is (eql 'outer (operator-of application-form/outer)))
          (is (eql 42 (value-of (first (body-of (definition-of application-form/outer)))))))))))

(def test test/semantics/flet/bug/1 ()
  (bind ((walked (walk-form '(flet ((outer ()
                                     ;; it's key for this test that body is empty
                                     ))
                              (outer))))
         (application-form/inner (finishes (first (body-of walked)))))
    (is (typep application-form/inner 'walked-lexical-application-form))))
