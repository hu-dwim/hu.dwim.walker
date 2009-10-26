;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.walker)

(def (class* e) implicit-progn-mixin ()
  ((body)))

(def print-object implicit-progn-mixin
  (format t "~A" (body-of -self-)))

(def (class* ea) implicit-progn-with-declarations-mixin (implicit-progn-mixin)
  ((declarations nil)))

(def (class* ea) binding-form-mixin ()
  ((bindings)))

(def (class* e) declaration-form (walked-form)
  ())

(def (class* ea) optimize-declaration-form (declaration-form)
  ((specification :accessor specification-of :initarg :specification)))

(def unwalker optimize-declaration-form (specification)
  `(optimize ,specification))

(def (class* e) variable-declaration-form (declaration-form
                                           named-walked-form)
  ())

(def (class* e) function-declaration-form (declaration-form
                                           named-walked-form)
  ())

(def (class* e) dynamic-extent-declaration-form (variable-declaration-form)
  ())

(def unwalker dynamic-extent-declaration-form (name)
  `(dynamic-extent ,name))

(def (class* e) ignorable-declaration-form-mixin (declaration-form)
  ())

(def (class* e) variable-ignorable-declaration-form (variable-declaration-form ignorable-declaration-form-mixin)
  ())

(def unwalker variable-ignorable-declaration-form (name)
  `(ignorable ,name))

(def (class* e) function-ignorable-declaration-form (function-declaration-form ignorable-declaration-form-mixin)
  ())

(def unwalker function-ignorable-declaration-form (name)
  `(ignorable (function ,name)))

(def (class* e) special-variable-declaration-form (variable-declaration-form)
  ())

(def unwalker special-variable-declaration-form (name)
  `(special ,name))

(def (class* e) type-declaration-form (variable-declaration-form)
  ((type :accessor type-of :initarg :type)))

(def unwalker type-declaration-form (type name)
  `(type ,type ,name))

(def (class* e) ftype-declaration-form (function-declaration-form)
  ((type :accessor type-of :initarg :type)))

(def unwalker ftype-declaration-form (type name)
  `(ftype ,type ,name))

(def (class* e) notinline-declaration-form (function-declaration-form)
  ())

(def unwalker notinline-declaration-form (name)
  `(notinline ,name))

(def (class* e) unknown-declaration-form (declaration-form)
  ((declaration-form :initarg :declaration-form :accessor declaration-form-of)))

(def unwalker unknown-declaration-form (declaration-form)
  declaration-form)

(defvar *known-declaration-types* (append
                                   #+sbcl
                                   '(sb-ext:muffle-conditions
                                     )
                                   ))

(defun walk-declaration (declaration parent environment)
  (let ((declares nil))
    (flet ((function-name (form)
             (if (and (consp form)
                      (eql (car form) 'function))
                 (second form)
                 nil)))
      (macrolet ((make-declaration (formclass &rest rest)
                   `(make-form-object ,formclass parent ,@rest))
                 (extend-env ((var list) newdeclare &rest datum)
                   `(dolist (,var ,list)
                      (push ,newdeclare declares)
                      (augment-walkenv! environment :declare ,@datum))))
        (bind (((type &rest arguments) declaration))
          (case type
            (dynamic-extent
             (extend-env (var arguments)
                         (make-declaration 'dynamic-extent-declaration-form :name var)
                         var `(dynamic-extent)))
            (ftype
             (extend-env (function-name (cdr arguments))
                         (make-form-object 'ftype-declaration-form parent
                                           :name function-name
                                           :type (first arguments))
                         function-name `(ftype ,(first arguments))))
            ((ignore ignorable)
             (extend-env (var arguments)
                         (aif (function-name var)
                              (make-declaration 'function-ignorable-declaration-form :name it)
                              (make-declaration 'variable-ignorable-declaration-form :name var))
                         var `(,type)))
            (inline
              (extend-env (function arguments)
                          (make-declaration 'function-ignorable-declaration-form :name function)
                          function `(inline)))
            (notinline
             (extend-env (function arguments)
                         (make-declaration 'notinline-declaration-form :name function)
                         function `(notinline)))
            (optimize
             (extend-env (optimize-spec arguments)
                         (make-declaration 'optimize-declaration-form :specification optimize-spec)
                         'optimize optimize-spec))
            (special
             (extend-env (var arguments)
                         (make-declaration 'special-variable-declaration-form :name var)
                         var `(special)))
            (type
             (extend-env (var (rest arguments))
                         (make-form-object 'type-declaration-form parent
                                           :name var
                                           :type (first arguments))
                         var `(type ,(first arguments))))
            (t
             (unless (member type *known-declaration-types* :test #'eq)
               (simple-style-warning "Ignoring unknown declaration ~S while walking forms. If it's a type declaration, then use the full form to avoid this warning: `(type ,type ,@variables), or you can also (pushnew ~S ~S)."
                                     declaration type '*known-declaration-types*))
             (push (make-form-object 'unknown-declaration-form parent
                                     :declaration-form declaration)
                   declares))))))
    (values environment declares)))

(defun unwalk-declarations (decls)
  ;; Return a list so declarations can be easily spliced.
  (if (null decls)
      nil
      (list `(declare ,@(unwalk-forms decls)))))

(def function walk-declarations (declarations parent env)
  (bind ((walked-declarations '()))
    (dolist (declaration declarations)
      (assert (eq (first declaration) 'declare))
      (dolist (entry (rest declaration))
        (with-current-form entry
          (bind ((walked-declaration nil))
            (setf (values env walked-declaration) (walk-declaration entry parent env))
            (appendf walked-declarations walked-declaration)))))
    walked-declarations))

(def function walk-implict-progn (parent forms env &key declarations-callback docstring-allowed declarations-allowed (whole *current-form*))
  (assert (and (typep parent 'implicit-progn-mixin)
               (or (not declarations-allowed)
                   (typep parent 'implicit-progn-with-declarations-mixin))))
  (bind (((:values body declarations docstring) (parse-body (coerce-to-form forms) :documentation docstring-allowed :whole whole)))
    (when docstring-allowed
      (setf (docstring-of parent) docstring))
    (when declarations
      (unless declarations-allowed
        (error "Declarations are not allowed at ~S" whole))
      (bind ((walked-declarations (walk-declarations declarations parent env)))
        (setf (declarations-of parent) walked-declarations)
        (when declarations-callback
          (setf env (funcall declarations-callback walked-declarations)))))
    (setf (body-of parent) (mapcar (lambda (form)
                                     (walk-form form :parent parent :environment env))
                                   (coerce-to-form body)))
    (values)))
