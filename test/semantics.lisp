;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.walker.test)

(defsuite* (test/semantics :in test))

(def test test/semantics/let/1 ()
  (bind ((walked (walk-form '(let ((foo 42)
                                   (spec 101))
                               (declare (special spec))
                               foo
                               spec
                               free)))
         (body (body-of walked))
         (binding-form (first (bindings-of walked)))
         (binding-value-form (initial-value-of binding-form))
         (variable-reference-form (first body)))
    (is (= 3 (length body)))
    (is (typep binding-value-form 'constant-form))
    (is (eql (value-of binding-value-form) 42))
    (is (typep variable-reference-form 'walked-lexical-variable-reference-form))
    (is (eq binding-form (definition-of variable-reference-form)))
    (is (typep (second body) 'special-variable-reference-form))
    (is (not (typep (second body) 'free-variable-reference-form)))
    (is (typep (third body) 'free-variable-reference-form))
    walked))

(def test test/semantics/let*/1 ()
  (bind ((walked (walk-form '(let* ((foo 42)
                                    (bar foo)
                                    (spec 101))
                               (declare (special spec))
                               foo
                               bar
                               spec
                               free)))
         (body (body-of walked))
         (bindings (bindings-of walked))
         (foo-binding-form (first bindings))
         (bar-binding-form (second bindings))
         (foo-reference-form (first body))
         (bar-reference-form (second body)))
    (is (= 4 (length body)))
    (is (typep foo-reference-form 'walked-lexical-variable-reference-form))
    (is (typep bar-reference-form 'walked-lexical-variable-reference-form))
    (is (typep (initial-value-of foo-binding-form) 'constant-form))
    (is (typep (initial-value-of bar-binding-form) 'walked-lexical-variable-reference-form))
    (is (eq (definition-of (initial-value-of bar-binding-form)) foo-binding-form))
    (is (eq (definition-of foo-reference-form) foo-binding-form))
    (is (eq (definition-of bar-reference-form) bar-binding-form))
    (is (typep (third body) 'special-variable-reference-form))
    (is (not (typep (third body) 'free-variable-reference-form)))
    (is (typep (fourth body) 'free-variable-reference-form))
    walked))

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
        (is (eql 2 (value-of (first (body-of walked-lexical-definition)))))))
    walked))

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
          (is (eql 42 (value-of (first (body-of (definition-of application-form/outer)))))))))
    walked))

(def test test/semantics/flet/bug/1 ()
  (bind ((walked (walk-form '(flet ((outer ()
                                     ;; it's key for this test that body is empty
                                     ))
                              (outer))))
         (application-form/inner (first (body-of walked))))
    (is (typep application-form/inner 'walked-lexical-application-form))
    walked))

(def test test/semantics/lambda/bug/1 ()
  (bind ((walked (not-signals warning (walk-form '(lambda (a)
                                                    a))))
         (variable-reference-form (first (body-of walked))))
    (is (typep variable-reference-form 'walked-lexical-variable-reference-form))
    walked))

(def test test/semantics/lambda/bug/2 ()
  (bind ((walked (walk-form '(lambda (&optional a (b 42)) ; no default value for A
                              )))
         (optional-argument-form (second (arguments-of walked)))
         (default-value-form (default-value-of optional-argument-form)))
    (is (null (default-value-of (first (arguments-of walked)))))
    (is (typep optional-argument-form 'optional-function-argument-form))
    (is (typep default-value-form 'constant-form))
    (is (eq (value-of default-value-form) 42))
    walked))

(def test test/semantics/lambda/bug/3 ()
  (not-signals undefined-variable-reference
    (walk-form '(lambda (a &optional (b a) &key (c b) &aux (d c) e (f e))
                 (values a b c d e f)))))
