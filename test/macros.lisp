;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.walker/test)

(defsuite* (test/macros :in test))

(deftest test/macros/macrolet ()
  (check-walk-unwalk '(macrolet ((+ (&body body)
                                  (reverse body)))
                       (+ 1 2 3 -))
                     '(locally (- 3 2 1)))

  (check-walk-unwalk '(macrolet ())
                     '(locally))

  (check-walk-unwalk '(macrolet ((+ (&body body)
                                  (reverse body)))
                       (princ "1111")
                       (+ 1 2 3 -))
                     '(locally
                       (princ "1111")
                       (- 3 2 1))))

(deftest test/macros/symbol-macrolet ()
  (check-walk-unwalk '(let ((obj 42))
                       (symbol-macrolet ((a (slot-value obj 'a))
                                         (b (slot-value obj 'b)))
                         (+ a b)))
                     '(let ((obj 42))
                       (locally
                           (+ (slot-value obj 'a) (slot-value obj 'b)))))

  (check-walk-unwalk '(symbol-macrolet ())
                     '(locally))

  (check-walk-unwalk '(let ((obj 42))
                       (symbol-macrolet ((a (slot-value obj 'a)))
                         (null a)
                         (/ a 2)))
                     '(let ((obj 42))
                       (locally
                           (null (slot-value obj 'a))
                         (/ (slot-value obj 'a) 2)))))

(deftest test/semantics/blocks/bug/1 ()
  (let ((*break-on-signals* t)) ; for errors inside macroexpand
    (eval
     '(macrolet ((test-walk (code &environment env)
                  (is (find-block-in-lexenv 'blk env))
                  (let ((walker-env (make-walk-environment env)))
                    (is (walk-environment/find walker-env :block 'blk
                                                          :otherwise :error))
                    (finishes
                      (walk-form code :environment walker-env)))))
       (block blk
         (test-walk
          (return-from blk)))))))

(deftest test/semantics/tags/bug/1 ()
  (let ((*break-on-signals* t)) ; for errors inside macroexpand
    (eval
     '(macrolet ((test-walk (code &environment env)
                  (is (find-tag-in-lexenv 'tagg env))
                  (let ((walker-env (make-walk-environment env)))
                    (is (walk-environment/find walker-env :tag 'tagg
                                                          :otherwise :error))
                    (finishes
                      (walk-form code :environment walker-env)))))
       (tagbody
          (test-walk
           (go tagg))
        tagg)))))
