;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See COPYING for details.

(in-package :cl-walker-test)

(defsuite* (test/lexenv :in test))

;; TODO would be better to use macroexpand-all for this, because stefil's errors get to cl:compile
;; which aborts at the first failure and skips the other assertions
(defun compile* (form)
  (handler-bind ((warning #'muffle-warning)
                 #+sbcl (sb-ext:compiler-note #'muffle-warning))
    (compile nil `(lambda ()
                    ,form)))
  (values))

(defun sort-by-symbol-name (sequence)
  (sort (copy-seq sequence)
        #'string< :key #'symbol-name))

(defsuite* (test/lexenv/query :in test/lexenv))

(deftest test/lexenv/query/variables ()
  (compile*
   `(symbol-macrolet ((a 42)
                      (b 43))
      (flet ((f1 () 1)
             (f2 () 2))
        (macrolet ((m1 () 1)
                   (m2 () 2))
          (let ((x 1)
                (y 2)
                (z 3))
            (declare (ignore z))
            (macrolet ((dummy (&environment env)
                         (is (equal '(x y)
                                    (sort-by-symbol-name
                                     (collect-variables-in-lexenv env))))
                         (is (equal '(x y z)
                                    (sort-by-symbol-name
                                     (collect-variables-in-lexenv env :include-ignored t))))
                         (is (find-variable-in-lexenv 'x env))
                         (is (not (find-variable-in-lexenv 'z env)))
                         (is (not (find-variable-in-lexenv 'a env)))
                         (is (not (find-variable-in-lexenv 'f1 env)))
                         (is (not (find-variable-in-lexenv 'm1 env)))
                         (is (find-variable-in-lexenv 'z env :include-ignored t))
                         nil))
              (dummy))))))))

(deftest test/lexenv/query/functions ()
  (compile*
   `(symbol-macrolet ((a 42)
                      (b 43))
      (flet ((f1 () 1)
             (f2 () 2))
        (macrolet ((m1 () 1)
                   (m2 () 2))
          (let ((x 1)
                (y 2)
                (z 3))
            (declare (ignore z))
            (macrolet ((dummy (&environment env)
                         (is (equal '(f1 f2)
                                    (sort-by-symbol-name
                                     (collect-functions-in-lexenv env))))
                         (is (find-function-in-lexenv 'f1 env))
                         (is (not (find-function-in-lexenv 'foo env)))
                         (is (not (find-function-in-lexenv 'a env)))
                         (is (not (find-function-in-lexenv 'm1 env)))
                         (is (not (find-function-in-lexenv 'dummy env)))
                         nil))
              (dummy))))))))

(deftest test/lexenv/query/macros ()
  (compile*
   `(symbol-macrolet ((a 42)
                      (b 43))
      (flet ((f1 () 1)
             (f2 () 2))
        (macrolet ((m1 () 1)
                   (m2 () 2))
          (let ((x 1)
                (y 2)
                (z 3))
            (declare (ignore z))
            (macrolet ((dummy (&environment env)
                         (is (equal '(dummy m1 m2)
                                    (sort-by-symbol-name
                                     (collect-macros-in-lexenv env))))
                         (is (find-macro-in-lexenv 'm1 env))
                         (is (not (find-macro-in-lexenv 'f1 env)))
                         (is (not (find-macro-in-lexenv 'a env)))
                         (is (not (find-macro-in-lexenv 'x env)))
                         nil))
              (dummy))))))))

(deftest test/lexenv/query/symbol-macros ()
  (compile*
   `(symbol-macrolet ((a 42)
                      (b 43))
      (flet ((f1 () 1)
             (f2 () 2))
        (macrolet ((m1 () 1)
                   (m2 () 2))
          (let ((x 1)
                (y 2)
                (z 3))
            (declare (ignore z))
            (macrolet ((dummy (&environment env)
                         (is (equal '(a b)
                                    (sort-by-symbol-name
                                     (collect-symbol-macros-in-lexenv env))))
                         (is (find-symbol-macro-in-lexenv 'a env))
                         (is (not (find-symbol-macro-in-lexenv 'm1 env)))
                         (is (not (find-symbol-macro-in-lexenv 'f1 env)))
                         (is (not (find-symbol-macro-in-lexenv 'x env)))
                         nil))
              (dummy))))))))

(defsuite* (test/lexenv/augment :in test/lexenv))

