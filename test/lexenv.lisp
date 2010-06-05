;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.walker.test)

(def suite* (test/lexenv :in test))

(def suite* (test/lexenv/query :in test/lexenv))

(def test test/lexenv/query/variables ()
  (with-captured-lexical-environment
      (env (symbol-macrolet ((a 42)
                             (b 43))
             (flet ((f1 () 1)
                    (f2 () 2))
               (macrolet ((m1 () 1)
                          (m2 () 2))
                 (let ((x 1)
                       (y 2)
                       (z 3))
                   (declare (ignore z))
                   -here-)))))
    (is (subsetp '(y x) (collect-variables-in-lexenv env)))
    (bind ((ignored 0)
           (non-ignored 0))
      (do-variables-in-lexenv (env name :ignored? ignored?)
        (when (eq (symbol-package name) *package*)
          (is (symbolp name))
          (if ignored?
              (incf ignored)
              (incf non-ignored))))
      (is (= ignored 1))
      (is (= non-ignored 2)))
    (is (subsetp '(z y x)
               (collect-variables-in-lexenv env :include-ignored? t)))
    (is (find-variable-in-lexenv 'x env))
    (is (not (find-variable-in-lexenv 'z env)))
    (is (not (find-variable-in-lexenv 'a env)))
    (is (not (find-variable-in-lexenv 'f1 env)))
    (is (not (find-variable-in-lexenv 'm1 env)))
    (is (find-variable-in-lexenv 'z env :include-ignored? t))))

(def test test/lexenv/query/functions ()
  (with-captured-lexical-environment
      (env (symbol-macrolet ((a 42)
                             (b 43))
             (flet ((f1 () 1)
                    (f2 () 2))
               (macrolet ((m1 () 1)
                          (m2 () 2))
                 (let ((x 1)
                       (y 2)
                       (z 3))
                   (declare (ignore z))
                   -here-)))))
    (is (equal '(f2 f1)
               (collect-functions-in-lexenv env)))
    (bind ((functions 0))
      (do-functions-in-lexenv (env name)
        (is (and (symbolp name)
                 (eq (symbol-package name) *package*)))
        (incf functions))
      (is (= functions 2)))
    (is (find-function-in-lexenv 'f1 env))
    (is (not (find-function-in-lexenv 'foo env)))
    (is (not (find-function-in-lexenv 'a env)))
    (is (not (find-function-in-lexenv 'm1 env)))
    (is (not (find-function-in-lexenv '-here- env)))))

(def test test/lexenv/query/macros ()
  (with-captured-lexical-environment
      (env (symbol-macrolet ((a 42)
                             (b 43))
             (flet ((f1 () 1)
                    (f2 () 2))
               (macrolet ((m1 () 1)
                          (m2 () 2))
                 (let ((x 1)
                       (y 2)
                       (z 3))
                   (declare (ignore z))
                   -here-)))))
    (is (subsetp '(m1 m2) (collect-macros-in-lexenv env)))
    (bind ((macros 0))
      (do-macros-in-lexenv (env name fn)
        (when (eq (symbol-package name) *package*)
          (is (symbolp name))
          (is (functionp fn))
          (incf macros)))
      (is (= macros 2)))
    (is (find-macro-in-lexenv 'm1 env))
    (is (not (find-macro-in-lexenv 'f1 env)))
    (is (not (find-macro-in-lexenv 'a env)))
    (is (not (find-macro-in-lexenv 'x env)))))

(deftest test/lexenv/query/symbol-macros ()
  (with-captured-lexical-environment
      (env (symbol-macrolet ((a 42)
                             (b 43))
             (flet ((f1 () 1)
                    (f2 () 2))
               (macrolet ((m1 () 1)
                          (m2 () 2))
                 (let ((x 1)
                       (y 2)
                       (z 3))
                   (declare (ignore z))
                   -here-)))))
    (is (set-equal (collect-symbol-macros-in-lexenv env)
                   '(a b)))
    (bind ((symbol-macros 0))
      (do-symbol-macros-in-lexenv (env name definition)
        (is (and (symbolp name)
                 (eq (symbol-package name) *package*)))
        (is (not (functionp definition)))
        (incf symbol-macros))
      (is (= symbol-macros 2)))
    (is (find-symbol-macro-in-lexenv 'a env))
    (is (not (find-symbol-macro-in-lexenv 'm1 env)))
    (is (not (find-symbol-macro-in-lexenv 'f1 env)))
    (is (not (find-symbol-macro-in-lexenv 'x env)))))

#-(or openmcl allegro)
(deftest test/lexenv/query/blocks ()
  (with-captured-lexical-environment
      (env (block b1
             (flet ((f1 () 1)
                    (f2 () 2))
               (block b2
                 (let ((x 1)
                       (y 2)
                       (z 3))
                   (declare (ignore z))
                   -here-)))))
    (is (equal '(b2 b1)
               (remove-if-not #'symbol-package
                              (collect-blocks-in-lexenv env))))
    (bind ((blocks 0))
      (do-blocks-in-lexenv (env name)
        (when (symbol-package name)
          (is (and (symbolp name)
                   (eq (symbol-package name) *package*)))
          (incf blocks)))
      (is (= blocks 2)))
    (is (find-block-in-lexenv 'b1 env))
    (is (not (find-block-in-lexenv '-here- env)))
    (is (not (find-block-in-lexenv 'f1 env)))
    (is (not (find-block-in-lexenv 'x env)))))

#-(or openmcl allegro)
(deftest test/lexenv/query/tags ()
  (with-captured-lexical-environment
      (env (block b1
             (tagbody
              t1
                (progn)
              t2
                (tagbody
                 t21
                   (progn)
                 t22
                   (block b2
                     -here-)))))
    (is (set-equal (collect-tags-in-lexenv env)
                   '(t21 t22 t1 t2)))
    (bind ((tags 0))
      (do-tags-in-lexenv (env name)
        (is (and (symbolp name)
                 (eq (symbol-package name) *package*)))
        (incf tags))
      (is (= tags 4)))
    (is (find-tag-in-lexenv 't1 env))
    (is (not (find-tag-in-lexenv '-here- env)))
    (is (not (find-tag-in-lexenv 'f1 env)))
    (is (not (find-tag-in-lexenv 'x env)))))

(def test test/lexenv/query/special-variable ()
  (with-captured-lexical-environment
      (env (let ((x 42))
             (declare (special x))
             -here-))
    (is (special-variable-name? 'x env))))

;; TODO (defsuite* (test/lexenv/augment :in test/lexenv))
