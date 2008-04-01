;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See COPYING for details.

(in-package :cl-walker)

(defclass application-form (form)
  ((operator :accessor operator-of :initarg :operator)
   (arguments :accessor arguments-of :initarg :arguments)))

(defunwalker-handler application-form (operator arguments)
  (cons operator (unwalk-forms arguments)))

(defclass local-application-form (application-form)
  ((code :accessor code-of :initarg :code)))

(defclass lexical-application-form (application-form)
  ())

(defclass free-application-form (application-form)
  ())

(defclass lambda-application-form (application-form)
  ())

(defunwalker-handler lambda-application-form (operator arguments)
  ;; The cadr is for getting rid of (function ...) which we can't have
  ;; at the beginning of a form.
  (cons (cadr (unwalk-form operator)) (unwalk-forms arguments)))

(defwalker-handler application (form parent env)
  (block nil
    (destructuring-bind (op &rest args)
        form
      (when (and (consp op)
                 (eq 'cl:lambda (car op)))
        (return
          (with-form-object (application lambda-application-form :parent parent :source form)
            (setf (operator-of application) (walk-form op application env)
                  (arguments-of application) (mapcar (lambda (form)
                                                       (walk-form form application env))
                                                     args)))))
      (when (lookup-walk-env env :macrolet op)
        (return (walk-form (funcall (lookup-walk-env env :macrolet op) form (cdr env)) parent env)))
      (when (and (symbolp op) (macro-function op))
        (multiple-value-bind (expansion expanded)
            (macroexpand-1 form (cdr env))
          (when expanded
            (return (walk-form expansion parent env)))))
      (let ((app (if (lookup-walk-env env :flet op)
                     (make-instance 'local-application-form :code (lookup-walk-env env :flet op))
                     (if (lookup-walk-env env :lexical-flet op)
                         (make-instance 'lexical-application-form)
                         (progn
                           (when (and *warn-undefined*
                                      (symbolp op)
                                      (not (fboundp op)))
                             (warn 'undefined-function-reference :name op))
                           (make-instance 'free-application-form))))))
        (setf (operator-of app) op
              (parent app) parent
              (source app) form
              (arguments-of app) (mapcar (lambda (form)
                                           (walk-form form app env))
                                         args))
        app))))

;;;; Functions

(defclass function-form (form)
  ())

(defclass lambda-function-form (function-form implicit-progn-with-declare-mixin)
  ((arguments :accessor arguments-of :initarg :arguments)))

(defunwalker-handler lambda-function-form (arguments body declares)
  `(function
    (lambda ,(unwalk-lambda-list arguments)
     ,@(unwalk-declarations declares)
     ,@(unwalk-forms body))))

(defclass function-object-form (form)
  ((name :accessor name-of :initarg :name)))

(defunwalker-handler function-object-form (name)
  `(function ,name))

(defclass local-function-object-form (function-object-form)
  ())

(defclass free-function-object-form (function-object-form)
  ())

(defclass lexical-function-object-form (function-object-form)
  ())

(defwalker-handler function (form parent env)
  (if (and (listp (second form))
           (eql 'cl:lambda (first (second form))))
      ;; (function (lambda ...))
      (walk-lambda (second form) parent env)
      ;; (function foo)
      (make-instance (if (lookup-walk-env env :flet (second form))
                         'local-function-object-form
                         (if (lookup-walk-env env :lexical-flet (second form))
                             'lexical-function-object-form
                             'free-function-object-form))
                     :name (second form)
                     :parent parent :source form)))

(defun walk-lambda (form parent env)
  (with-form-object (func lambda-function-form
                          :parent parent
                          :source form)
    ;; 1) parse the argument list creating a list of FUNCTION-ARGUMENT-FORM objects
    (multiple-value-setf ((arguments-of func) env)
      (walk-lambda-list (second form) func env))
    ;; 2) parse the body
    (multiple-value-setf ((body func) nil (declares func))
      (walk-implict-progn func (cddr form) env :declare t))
    ;; all done
    func))

(defun walk-lambda-list (lambda-list parent env &key allow-specializers macro-p)
  (declare (ignore macro-p))
  (flet ((extend-env (argument)
           (unless (typep argument 'allow-other-keys-function-argument-form)
             (extend-walk-env env :let (name-of argument) argument))))
    (let ((state :required)
          (arguments '()))
      (dolist (argument lambda-list)
        (if (member argument '(&optional &key &rest))
            (setf state argument)
            (progn
              (push (case state
                      (:required
                       (if allow-specializers
                           (walk-specialized-argument-form argument parent env)
                           (walk-required-argument argument parent env)))
                      (&optional (walk-optional-argument argument parent env))
                      (&key
                       (if (eql '&allow-other-keys argument)
                           (make-instance 'allow-other-keys-function-argument-form
                                          :parent parent :source argument)
                           (walk-keyword-argument argument parent env)))
                      (&rest (walk-rest-argument argument parent env)))
                    arguments)
              (extend-env (car arguments)))))
      (values (nreverse arguments) env))))

(defclass function-argument-form (form)
  ((name :accessor name-of :initarg :name)))

(defmethod print-object ((argument function-argument-form) stream)
  (print-unreadable-object (argument stream :type t :identity t)
    (if (slot-boundp argument 'name)
        (format stream "~S" (name-of argument))
        (write-string "#<unbound name>" stream))))

(defclass required-function-argument-form (function-argument-form)
  ())

(defun walk-required-argument (form parent env)
  (declare (ignore env))
  (make-instance 'required-function-argument-form
                 :name form
                 :parent parent :source form))

(defunwalker-handler required-function-argument-form (name)
  name)

(defclass specialized-function-argument-form (required-function-argument-form)
  ((specializer :accessor specializer :initarg :specializer)))

(defun walk-specialized-argument-form (form parent env)
  (declare (ignore env))
  (make-instance 'specialized-function-argument-form
                 :name (if (listp form)
                           (first form)
                           form)
                 :specializer (if (listp form)
                                  (second form)
                                  'T)
                 :parent parent
                 :source form))

(defunwalker-handler specialized-function-argument-form (name specializer)
  (if (eq specializer t)
      name
      `(,name ,specializer)))

(defclass optional-function-argument-form (function-argument-form)
  ((default-value :accessor default-value :initarg :default-value)
   (supplied-p-parameter :accessor supplied-p-parameter :initarg :supplied-p-parameter)))

(defun walk-optional-argument (form parent env)
  (destructuring-bind (name &optional default-value supplied-p-parameter)
      (ensure-list form)
    (with-form-object (arg optional-function-argument-form
                           :parent parent
                           :source form
                           :name name
                           :supplied-p-parameter supplied-p-parameter)
      (setf (default-value arg) (walk-form default-value arg env)))))

(defunwalker-handler optional-function-argument-form (name default-value supplied-p-parameter)
  (let ((default-value (unwalk-form default-value)))
    (cond ((and name default-value supplied-p-parameter)
           `(,name ,default-value ,supplied-p-parameter))
          ((and name default-value)
           `(,name ,default-value))
          (name name)
          (t (error "Invalid optional argument")))))

(defclass keyword-function-argument-form (function-argument-form)
  ((keyword-name :accessor keyword-name :initarg :keyword-name)
   (default-value :accessor default-value :initarg :default-value)
   (supplied-p-parameter :accessor supplied-p-parameter :initarg :supplied-p-parameter)))

(defun effective-keyword-name (k)
  (or (keyword-name k)
      (intern (symbol-name (name-of k)) :keyword)))

(defun walk-keyword-argument (form parent env)
  (destructuring-bind (name &optional default-value supplied-p-parameter)
      (ensure-list form)
    (let ((name (if (consp name)
                    (second name)
                    name))
          (keyword (if (consp name)
                       (first name)
                       nil)))
      (with-form-object (arg keyword-function-argument-form
                             :parent parent
                             :source form
                             :name name
                             :keyword-name keyword
                             :supplied-p-parameter supplied-p-parameter)
        (setf (default-value arg) (walk-form default-value arg env))))))

(defunwalker-handler keyword-function-argument-form (keyword-name name default-value supplied-p-parameter)
  (let ((default-value (unwalk-form default-value)))
    (cond ((and keyword-name name default-value supplied-p-parameter)
           `((,keyword-name ,name) ,default-value ,supplied-p-parameter))
          ((and name default-value supplied-p-parameter)
           `(,name ,default-value ,supplied-p-parameter))
          ((and name default-value)
           `(,name ,default-value))
          (name name)
          (t (error "Invalid keyword argument")))))

(defclass allow-other-keys-function-argument-form (function-argument-form)
  ())

(defunwalker-handler allow-other-keys-function-argument-form ()
  '&allow-other-keys)

(defclass rest-function-argument-form (function-argument-form)
  ())

(defun walk-rest-argument (form parent env)
  (declare (ignore env))
  (make-instance 'rest-function-argument-form :name form
                 :parent parent :source form))

(defunwalker-handler rest-function-argument-form (name)
  name)

(defun unwalk-lambda-list (arguments)
  (let (optional-p rest-p keyword-p)
    (mapcan #'(lambda (form)
                (append
                 (typecase form
                   (optional-function-argument-form
                    (unless optional-p
                      (assert (not keyword-p))
                      (assert (not rest-p))
                      (setq optional-p t)
                      '(&optional)))
                   (rest-function-argument-form
                    (unless rest-p
                      (assert (not keyword-p))
                      (setq rest-p t)
                      '(&rest)))
                   (keyword-function-argument-form
                    (unless keyword-p
                      (setq keyword-p t)
                      '(&key))))
                 (list (unwalk-form form))))
            arguments)))

;;;; FLET/LABELS

(defclass function-binding-form (form binding-form-mixin implicit-progn-with-declare-mixin)
  ())

(defclass flet-form (function-binding-form)
  ())

(defwalker-handler flet (form parent env)
  (destructuring-bind (binds &body body)
      (cdr form)
    (with-form-object (flet flet-form :parent parent :source form)
      ;; build up the objects for the bindings in the original env
      (loop
         :for (name args . body) :in binds
         :collect (cons name (walk-form `(lambda ,args ,@body) flet env)) :into bindings
         :finally (setf (binds flet) bindings))
      ;; walk the body in the new env
      (multiple-value-setf ((body flet) _ (declares flet))
        (walk-implict-progn flet
                            body
                            (loop
                               with env = env
                               for (name . lambda) in (binds flet)
                               do (extend-walk-env env :flet name lambda)
                               finally (return env))
                            :declare t)))))

;; TODO factor out stuff in flet-form and labels-form
(defunwalker-handler flet-form (binds body declares)
  (flet ((unwalk-flet (binds)
           (mapcar #'(lambda (bind)
                       (cons (car bind)
                             ;; remove (function (lambda ...)) of the function bindings
                             (cdadr (unwalk-form (cdr bind)))))
                   binds)))
    `(flet ,(unwalk-flet binds)
       ,@(unwalk-declarations declares)
       ,@(unwalk-forms body))))

(defclass labels-form (function-binding-form)
  ())

(defwalker-handler labels (form parent env)
  (destructuring-bind (binds &body body)
      (cdr form)
    (with-form-object (labels labels-form :parent parent :source form :binds '())
      ;; we need to walk over the bindings twice. the first pass
      ;; creates some 'empty' lambda objects in the environment so
      ;; that local-application-form and local-function-object-form
      ;; have something to point to. the second pass then walks the
      ;; actual bodies of the form filling in the previously created
      ;; objects.
      (loop
         :for (name arguments . body) :in binds
         :for lambda = (make-instance 'lambda-function-form
                                      :parent labels
                                      :source (list* name arguments body))
         :do (progn
               (push (cons name lambda) (binds labels))
               (extend-walk-env env :flet name lambda)))
      (setf (binds labels) (nreverse (binds labels)))
      (loop
         :for form :in binds
         :for (arguments . body) = (cdr form)
         :for binding :in (binds labels)
         :for lambda = (cdr binding)
         :for tmp-lambda = (walk-lambda `(lambda ,arguments ,@body) labels env)
         :do (setf (body lambda) (body tmp-lambda)
                   (arguments-of lambda) (arguments-of tmp-lambda)
                   (declares lambda) (declares tmp-lambda)))
      (multiple-value-setf ((body labels) _ (declares labels))
        (walk-implict-progn labels body env :declare t)))))

(defunwalker-handler labels-form (binds body declares)
  (flet ((unwalk-labels (binds)
           (mapcar #'(lambda (bind)
                       (cons (car bind)
                             ;; remove (function (lambda ...)) of the function bindings
                             (cdadr (unwalk-form (cdr bind)))))
                   binds)))
    `(labels ,(unwalk-labels binds)
       ,@(unwalk-declarations declares)
       ,@(unwalk-forms body))))

