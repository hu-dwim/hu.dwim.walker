;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.walker.test)

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
