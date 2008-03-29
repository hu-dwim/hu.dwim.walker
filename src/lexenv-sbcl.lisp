;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See COPYING for details.

(in-package :cl-walker)

;;;
;;; SBCL
;;;
(defun make-empty-lexenv ()
  (sb-kernel:make-null-lexenv))

(defun iterate-variables-in-lexenv (visitor lexenv &key include-ignored)
  (loop
     :for entry :in (sb-c::lexenv-vars lexenv)
     :for name = (first entry)
     :for definition = (rest entry)
     :for ignored? = (and (typep definition 'sb-c::lambda-var)
                          (sb-c::lambda-var-ignorep definition))
     :unless (and (consp definition)
                  (eq 'sb-sys::macro (first definition)))
     :do (when (or (not ignored?)
                   include-ignored)
           (funcall visitor name ignored?))))

(defun iterate-functions-in-lexenv (visitor lexenv)
  (loop
     :for entry :in (sb-c::lexenv-funs lexenv)
     :for name = (first entry)
     :for definition = (rest entry)
     :unless (and (consp definition)
                  (eq 'sb-sys::macro (first definition)))
     :do (funcall visitor name)))

(defun iterate-macros-in-lexenv (visitor lexenv)
  (loop
     :for entry :in (sb-c::lexenv-funs lexenv)
     :for name = (first entry)
     :for definition = (rest entry)
     :for macro? = (and (consp definition)
                        (eq 'sb-sys::macro (first definition)))
     :for macro-function = (when macro?
                             (rest definition))
     :when macro?
     :do (progn
           (assert (functionp macro-function))
           (funcall visitor name macro-function))))

(defun iterate-symbol-macros-in-lexenv (visitor lexenv)
  (loop
     :for entry :in (sb-c::lexenv-vars lexenv)
     :for name = (first entry)
     :for definition = (rest entry)
     :for macro? = (and (consp definition)
                        (eq 'sb-sys::macro (first definition)))
     :for macro-body = (when macro?
                         (rest definition))
     :when macro?
     :do (funcall visitor name macro-body)))

(defun augment-with-variable (lexenv name)
  (sb-c::make-lexenv :default lexenv :vars (list (cons name t))))

(defun augment-with-function (lexenv name)
  (sb-c::make-lexenv :default lexenv :funs (list (cons name t))))

(defun augment-with-macro (lexenv name def)
  (sb-c::make-lexenv :default lexenv :funs (list (list* name 'sb-sys::macro def))))

(defun augment-with-symbol-macro (lexenv name def)
  (sb-c::make-lexenv :default lexenv :vars (list (list* name 'sb-sys::macro def))))

