;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See COPYING for details.

(in-package :cl-walker)

(defclass declaration-form (form)
  ())

(defclass optimize-declaration-form (declaration-form)
  ((specification :accessor specification-of :initarg :specification)))

(defunwalker-handler optimize-declaration-form (specification)
  `(optimize ,specification))

(defclass variable-declaration-form (declaration-form)
  ((name :accessor name-of :initarg :name)))

(defclass function-declaration-form (declaration-form)
  ((name :accessor name-of :initarg :name)))

(defclass dynamic-extent-declaration-form (variable-declaration-form)
  ())

(defunwalker-handler dynamic-extent-declaration-form (name)
  `(dynamic-extent ,name))

(defclass ignorable-declaration-form-mixin (declaration-form)
  ())

(defclass variable-ignorable-declaration-form (variable-declaration-form ignorable-declaration-form-mixin)
  ())

(defunwalker-handler variable-ignorable-declaration-form (name)
  `(ignorable ,name))

(defclass function-ignorable-declaration-form (function-declaration-form ignorable-declaration-form-mixin)
  ())

(defunwalker-handler function-ignorable-declaration-form (name)
  `(ignorable (function ,name)))

(defclass special-declaration-form (variable-declaration-form)
  ())

(defunwalker-handler special-declaration-form (name)
  `(special ,name))

(defclass type-declaration-form (variable-declaration-form)
  ((type :accessor type-of :initarg :type)))

(defunwalker-handler type-declaration-form (type name)
  `(type ,type ,name))

(defclass ftype-declaration-form (function-declaration-form)
  ((type :accessor type-of :initarg :type)))

(defunwalker-handler ftype-declaration-form (type name)
  `(ftype ,type ,name))

(defclass notinline-declaration-form (function-declaration-form)
  ())

(defunwalker-handler notinline-declaration-form (name)
  `(notinline ,name))

(defclass unknown-declaration-form (declaration-form)
  ())

(defunwalker-handler unknown-declaration-form (source)
  source)

(defun parse-declaration (declaration environment parent)
  (let ((declares nil))
    (flet ((function-name (form)
             (if (and (consp form)
                      (eql (car form) 'function))
                 (second form)
                 nil)))
      (macrolet ((make-declaration (varname formclass &rest rest)
                   `(make-instance ,formclass :parent parent :source (list type ,varname) ,@rest))
                 (extend-env ((var list) newdeclare &rest datum)
                   `(dolist (,var ,list)
                      (push ,newdeclare declares)
                      (augment-walkenv! environment :declare ,@datum))))
        (destructuring-bind (type &rest arguments)
            declaration
          (case type
            (dynamic-extent
             (extend-env (var arguments)
                         (make-declaration var 'dynamic-extent-declaration-form :name var)
                         var `(dynamic-extent)))
            (ftype
             (extend-env (function-name (cdr arguments))
                         (make-instance 'ftype-declaration-form
                                        :parent parent
                                        :source `(ftype ,(first arguments) function-name)
                                        :name function-name
                                        :type (first arguments))
                         function-name `(ftype ,(first arguments))))
            ((ignore ignorable)
             (extend-env (var arguments)
                         (aif (function-name var)
                              (make-declaration var 'function-ignorable-declaration-form :name it)
                              (make-declaration var 'variable-ignorable-declaration-form :name var))
                         var `(,type)))
            (inline
              (extend-env (function arguments)
                          (make-declaration function 'function-ignorable-declaration-form :name function)
                          function `(inline)))
            (notinline
             (extend-env (function arguments)
                         (make-declaration function 'notinline-declaration-form :name function)
                         function `(notinline)))
            (optimize
             (extend-env (optimize-spec arguments)
                         (make-declaration optimize-spec 'optimize-declaration-form :specification optimize-spec)
                         'optimize optimize-spec))
            (special
             (extend-env (var arguments)
                         (make-declaration var 'special-declaration-form :name var)
                         var `(special)))
            (type
             (extend-env (var (rest arguments))
                         (make-instance 'type-declaration-form
                                        :parent parent
                                        :source `(type ,(first arguments) ,var)
                                        :name var
                                        :type (first arguments))
                         var `(type ,(first arguments))))
            (t
             (simple-style-warning "Ignoring unknown declaration ~S while walking forms. If it's a type declaration, then use the full form to avoid the warning: `(type ,type ,@variables)!"
                                   declaration)
             (push (make-instance 'unknown-declaration-form :parent parent :source declaration) declares))))))
    (values environment declares)))

(defun unwalk-declarations (decls)
  ;; Return a list so declarations can be easily spliced.
  (if (null decls)
      nil
      (list `(declare ,@(unwalk-forms decls)))))

(defun walk-implict-progn (parent forms env &key docstring declare)
  (handler-bind ((undefined-reference
                  (lambda (condition)
                    (unless (enclosing-code-of condition)
                      (setf (enclosing-code-of condition) `(progn ,@forms))))))
    (multiple-value-bind (body env docstring declarations)
        (split-body forms env :parent parent :docstring docstring :declare declare)
      (values (mapcar (lambda (form)
                        (walk-form form parent env))
                      body)
              docstring
              declarations))))

