;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.walker/test)

(defsuite* (test/utils :in test))

(def test test-collect-variable-references (form expected-count)
  (with-active-layers (ignore-undefined-references)
    (bind ((ast (walk-form form)))
      (is (= expected-count
             (length (collect-variable-references ast)))))))

(def test test/utils/collect-variable-references/1 ()
  (loop
     :for (form expected-count) :in
     '((var 1)
       ((fn var1 var2) 2)
       ((progn (+ var1 var2)) 2)
       ((flet ((fn (x)
                 (+ x a)))
          (+ var1 (fn var2)))
        4)
       ((macrolet ((mac (x)
                     `(+ ,x a)))
          (mac (+ var1 var2)))
        3)
       ((let ((a (+ b c)))
          a)
        3))
     :do (test-collect-variable-references form expected-count)))

(def test test/utils/attrs/1 ()
  (let ((walked (walk-form :foo)))
    (is (eql (form-attribute walked :test :none) :none))
    (setf (form-attribute walked :test) :set)
    (is (eql (form-attribute walked :test) :set))
    walked))

(def test test/utils/substitute/1 ()
  (let* ((walked (walk-form '(let* ((a 5)
                                    (b a))
                              (+ a b))))
         (body (first (body-of walked)))
         (a-def (first (bindings-of walked)))
         (b-def (second (bindings-of walked)))
         (a-arg (first (arguments-of body)))
         (b-arg (second (arguments-of body)))
         (subst1 (substitute-ast-if a-arg
                                    (lambda (form)
                                      (and (typep form 'lexical-variable-reference-form)
                                           (eq (definition-of form) b-def)))
                                    walked))
         (body1 (first (body-of subst1)))
         (a-def1 (first (bindings-of subst1)))
         (b-def1 (second (bindings-of subst1)))
         (a-arg1 (first (arguments-of body1)))
         (b-arg1 (second (arguments-of body1))))
    (is (eq (definition-of a-arg) a-def))
    (is (eq (first (arguments-of body)) a-arg))
    (is (eq (second (arguments-of body)) b-arg))
    (is (not (eq subst1 walked)))
    (is (not (eq (initial-value-of b-def1) a-arg)))
    (is (not (eq a-arg1 a-arg)))
    (is (not (eq b-arg1 a-arg)))
    (is (not (eq b-arg1 a-arg1)))
    (is (eq (definition-of (initial-value-of b-def1)) a-def1))
    (is (eq (parent-of (initial-value-of b-def1)) b-def1))
    (is (eq (definition-of a-arg1) a-def1))
    (is (eq (definition-of b-arg1) a-def1))
    (is (eq (parent-of b-arg1) body1))))

(def test test/utils/substitute/2 ()
  (let* ((walked (walk-form '(let* ((a 5)
                                    (b a))
                              (+ a b))))
         (body (first (body-of walked)))
         (a-def (first (bindings-of walked)))
         (b-def (second (bindings-of walked)))
         (a-arg (first (arguments-of body)))
         (a-arg-copy (hu.dwim.walker::copy-ast-form a-arg))
         (subst1 (substitute-ast-if a-arg-copy
                                    (lambda (form)
                                      (and (typep form 'lexical-variable-reference-form)
                                           (eq (definition-of form) b-def)))
                                    walked
                                    :in-place t
                                    :first-without-copy t))
         (b-arg1 (second (arguments-of body))))
    (is (eq subst1 walked))
    (is (eq (first (body-of walked)) body))
    (is (eq (first (arguments-of body)) a-arg))
    (is (eq (definition-of (initial-value-of b-def)) a-def))
    (is (eq (parent-of (initial-value-of b-def)) b-def))
    (is (eq (definition-of b-arg1) a-def))
    (is (eq (parent-of b-arg1) body))
    (is (or (eq (initial-value-of b-def) a-arg-copy)
            (eq b-arg1 a-arg-copy)))))
