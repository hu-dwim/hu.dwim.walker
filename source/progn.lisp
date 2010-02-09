;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.walker)

(def form-class implicit-progn-mixin ()
  ((body :ast-link t)))

(def print-object implicit-progn-mixin
  (format t "~A" (body-of -self-)))

(def form-class implicit-progn-with-declarations-mixin (implicit-progn-mixin)
  ((declarations nil :ast-link t)))

(def form-class binder-form-mixin ()
  ((bindings :ast-link t)))

(def form-class declaration-form ()
  ())

(def form-class optimize-declaration-form (declaration-form)
  ((specification :accessor specification-of :initarg :specification)))

(def unwalker optimize-declaration-form (specification)
  `(optimize ,specification))

(def form-class variable-declaration-form (declaration-form
                                           named-walked-form)
  ())

(def form-class function-declaration-form (declaration-form
                                           named-walked-form)
  ())

(def form-class dynamic-extent-declaration-form (variable-declaration-form)
  ())

(def unwalker dynamic-extent-declaration-form (name)
  `(dynamic-extent ,name))

(def form-class ignorable-declaration-form-mixin (declaration-form)
  ())

(def form-class variable-ignorable-declaration-form (variable-declaration-form ignorable-declaration-form-mixin)
  ())

(def unwalker variable-ignorable-declaration-form (name)
  `(ignorable ,name))

(def form-class function-ignorable-declaration-form (function-declaration-form ignorable-declaration-form-mixin)
  ())

(def unwalker function-ignorable-declaration-form (name)
  `(ignorable (function ,name)))

(def form-class special-variable-declaration-form (variable-declaration-form)
  ())

(def unwalker special-variable-declaration-form (name)
  `(special ,name))

(def form-class type-declaration-form (variable-declaration-form)
  ((declared-type)))

(def unwalker type-declaration-form (declared-type name)
  `(type ,declared-type ,name))

(def form-class ftype-declaration-form (function-declaration-form)
  ((declared-type)))

(def unwalker ftype-declaration-form (declared-type name)
  `(ftype ,declared-type ,name))

(def form-class notinline-declaration-form (function-declaration-form)
  ())

(def unwalker notinline-declaration-form (name)
  `(notinline ,name))

(def form-class unknown-declaration-form (declaration-form)
  ((declaration-form :initarg :declaration-form :accessor declaration-form-of)))

(def method name-of ((self unknown-declaration-form))
  (first (declaration-form-of self)))

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
                      (augment-walk-environment! environment :declare ,@datum))))
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
                                           :declared-type (first arguments))
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
                                           :declared-type (first arguments))
                         var `(type ,(first arguments))))
            (t
             (unless (member type *known-declaration-types* :test #'eq)
               (simple-walker-style-warning "Ignoring unknown declaration ~S while walking forms. If it's a type declaration, then use the full form to avoid this warning: `(type ,type ,@variables), or you can also (pushnew ~S ~S)."
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

(def function augment-with-special-vars (env declarations)
  (reduce (lambda (env form)
            (if (typep form 'special-variable-declaration-form)
                (let* ((name (name-of form))
                       (type-form (find-form-by-name name declarations
                                                     :type 'type-declaration-form))
                       (type (if type-form (declared-type-of type-form))))
                  (augment-walk-environment env :unwalked-variable name (cons :special type)))
                env))
          declarations :initial-value env))

(def (function e) walk-implict-progn (parent forms env &key declarations-callback docstring-allowed declarations-allowed (whole *current-form*))
  (assert (and (typep parent 'implicit-progn-mixin)
               (or (not declarations-allowed)
                   (typep parent 'implicit-progn-with-declarations-mixin))))
  (check-type env walk-environment)
  (bind (((:values body declarations docstring) (parse-body (coerce-to-form forms) :documentation docstring-allowed :whole whole)))
    (when docstring-allowed
      (setf (docstring-of parent) docstring))
    (when (and declarations
               (not declarations-allowed))
      (error "Declarations are not allowed at ~S" whole))
    (when declarations-allowed
      (bind ((walked-declarations (walk-declarations declarations parent env)))
        (setf (declarations-of parent) walked-declarations)
        (when declarations-callback
          ;; always call declarations-callback because some crucial sideffects may happen inside them (like in LET's walker)
          (setf env (funcall declarations-callback walked-declarations)))
        ;; Add special declarations to the environment
        (setf env (augment-with-special-vars env walked-declarations))))
    (setf (body-of parent) (mapcar (lambda (form)
                                     (walk-form form :parent parent :environment env))
                                   (coerce-to-form body)))
    (values)))
