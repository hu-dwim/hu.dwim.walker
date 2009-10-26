;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.walker)

(def (class* e) application-form (walked-form)
  ((operator)
   (arguments)))

(def unwalker application-form (operator arguments)
  (cons operator (recurse-on-body arguments)))

(def print-object application-form
  ;; the bang sign is a weak try... but at least mark it somehow that it's not a normal sexp...
  (princ "!(")
  (princ (operator-of -self-))
  (princ " ")
  (let ((first t))
    (dolist (arg (arguments-of -self-))
      (unless first
        (princ " "))
      (princ arg)
      (setf first nil)))
  (princ ")"))

(def (class* e) lexical-application-form (application-form)
  ((code)))

(def (class* e) walked-lexical-application-form (lexical-application-form)
  ())

(def (class* e) unwalked-lexical-application-form (lexical-application-form)
  ())

(def (class* e) free-application-form (application-form)
  ())

(def (class* e) lambda-application-form (application-form)
  ())

(def unwalker lambda-application-form (operator arguments)
  ;; KLUDGE: The cadr is for getting rid of (function ...) which we can't have at the beginning of a form.
  (cons (cadr (recurse operator)) (recurse-on-body arguments)))

(def walker application
  (bind (((operator &rest args) -form-))
    (flet ((walk-arguments (application-form)
             (loop
                for index :from 1
                for arg :in args
                collect (recurse arg application-form))))
      (when (lambda-form? operator)
        (return
          (with-form-object (application 'lambda-application-form -parent-)
            (setf (operator-of application) (walk-lambda operator application -environment-)
                  (arguments-of application) (walk-arguments application)))))
      (bind ((lexenv (cdr -environment-))
             ((:values innermost-lexical-definition-type nil expander) (-lookup- nil operator)))
        (awhen (eq :macro innermost-lexical-definition-type)
          (bind ((*inside-macroexpansion* t)
                 (expansion (funcall expander -form- lexenv)))
            (return (recurse expansion))))
        (when (and (symbolp operator)
                   (macro-name? operator lexenv)
                   (not (member innermost-lexical-definition-type '(:function :unwalked-function))))
          (bind (((:values expansion expanded?) (walker-macroexpand-1 -form- lexenv)))
            (when expanded?
              (bind ((*inside-macroexpansion* t))
                (return (recurse expansion)))))))
      (bind ((application-form (aif (-lookup- :function operator)
                                    (make-instance 'walked-lexical-application-form :code it)
                                    (if (-lookup- :unwalked-function operator)
                                        (make-instance 'unwalked-lexical-application-form)
                                        (progn
                                          (when (and (symbolp operator)
                                                     (not (function-name? operator)))
                                            (handle-undefined-reference :function operator))
                                          (make-instance 'free-application-form))))))
        (setf (operator-of application-form) operator)
        (setf (parent-of application-form) -parent-)
        (setf (source-of application-form) -form-)
        (setf (arguments-of application-form) (walk-arguments application-form))
        application-form))))

;;;; Functions

(def (class* e) function-form (walked-form)
  ())

(def (class* e) lambda-function-form (function-form implicit-progn-with-declare-mixin)
  ((arguments)))

(def unwalker lambda-function-form (arguments body declares)
  `#'(lambda ,(unwalk-ordinary-lambda-list arguments)
       ,@(unwalk-declarations declares)
       ,@(recurse-on-body body)))

(def (class* e) function-definition-form (lambda-function-form)
  ((name)))

(def walker defun
  (with-form-object (node 'function-definition-form -parent-
                          :name (second -form-))
    ;; TODO finish fixing the defun docstring bug...
    #+nil
    (bind (((:values remaining-forms declarations doc-string) (parse-body (nthcdr 3 form) :documentation t :whole form)))
      (walk-lambda-like node (third form) remaining-forms env
                        :declarations declarations
                        :documentation doc-string))
    (walk-lambda-like node (third -form-)
                      (nthcdr 3 -form-) -environment-)))

(def unwalker function-definition-form (form name arguments body declares)
  `(defun ,name ,(unwalk-ordinary-lambda-list arguments)
     ,@(unwalk-declarations declares)
     ,@(recurse-on-body body)))

(def (class* e) named-lambda-function-form (lambda-function-form)
  ((special-form)
   (name)))

(def unwalker named-lambda-function-form (special-form name arguments body declares)
  `(function
    (,special-form ,name ,(unwalk-ordinary-lambda-list arguments)
     ,@(unwalk-declarations declares)
     ,@(recurse-on-body body))))

(def (class* e) function-object-form (walked-form)
  ((name)))

(def unwalker function-object-form (name)
  `(function ,name))

(def (class* e) lexical-function-object-form (function-object-form)
  ())

(def (class* e) walked-lexical-function-object-form (lexical-function-object-form)
  ())

(def (class* e) unwalked-lexical-function-object-form (lexical-function-object-form)
  ())

(def (class* e) free-function-object-form (function-object-form)
  ())

(def walker function
  (cond
    ((lambda-form? (second -form-))
     ;; specially handling the list (function (lambda ...))
     (walk-lambda (second -form-) -parent- -environment-))
    #+sbcl
    ((and (consp (second -form-))
          (eq (first (second -form-)) 'sb-int:named-lambda))
     (bind ((named-lambda-form (second -form-)))
       (with-form-object (node 'named-lambda-function-form -parent-
                               :special-form (first named-lambda-form)
                               :name (second named-lambda-form))
         (walk-lambda-like node (third named-lambda-form)
                           (nthcdr 3 named-lambda-form) -environment-))))
    (t
     ;; (function foo)
     (bind ((function-name (second -form-)))
       (make-form-object (if (-lookup- :function function-name)
                             'walked-lexical-function-object-form
                             (if (-lookup- :unwalked-function function-name)
                                 'unwalked-lexical-function-object-form
                                 'free-function-object-form))
                         -parent-
                         :name function-name)))))

(defun walk-lambda (form parent env)
  (with-form-object (ast-node 'lambda-function-form parent)
    (walk-lambda-like ast-node (second form) (cddr form) env)))

(def layered-function walk-lambda-like (ast-node args body env)
  (:method (ast-node args body env)
    (setf (values (arguments-of ast-node) env) (walk-ordinary-lambda-list args ast-node env))
    (walk-implict-progn ast-node body env :declare t)
    ast-node))

(def (function e) walk-ordinary-lambda-list (lambda-list parent env &key allow-specializers)
  (bind (((:values requireds optionals rest keywords allow-other-keys? auxiliaries) (parse-ordinary-lambda-list lambda-list :normalize nil))
         (result (nconc
                  (loop
                     :for required :in requireds
                     :collect (if allow-specializers
                                  (walk-specialized-argument required parent env)
                                  (make-form-object 'required-function-argument-form parent :name required)))
                  (loop
                     :for optional :in optionals
                     :collect (walk-optional-argument optional parent env))
                  (when rest
                    (list (make-form-object 'rest-function-argument-form parent :name rest)))
                  (loop
                     :for keyword :in keywords
                     :collect (walk-keyword-argument keyword parent env))
                  (when allow-other-keys?
                    (list (make-form-object 'allow-other-keys-function-argument-form parent)))
                  (loop
                     :for auxiliary :in auxiliaries
                     :collect (walk-auxiliary-argument auxiliary parent env)))))
    (dolist (parsed result)
      (unless (typep parsed 'allow-other-keys-function-argument-form)
        (augment-walkenv! env :variable (name-of parsed) parsed)))
    result))

(def (class* e) function-argument-form (walked-form)
  ((name)))

(def print-object function-argument-form
  (format t "~S" (name-of -self-)))

(def (class* e) required-function-argument-form (function-argument-form)
  ())

(def unwalker required-function-argument-form (name)
  name)

(def (class* e) specialized-function-argument-form (required-function-argument-form)
  ((specializer)))

(def function walk-specialized-argument (form parent env)
  (declare (ignore env))
  (make-form-object 'specialized-function-argument-form parent
                    :name (if (listp form)
                              (first form)
                              form)
                    :specializer (if (listp form)
                                     (second form)
                                     t)))

(def unwalker specialized-function-argument-form (name specializer)
  (if (eq specializer t)
      name
      `(,name ,specializer)))

(def (class* e) optional-function-argument-form (function-argument-form)
  ((default-value nil)
   (supplied-p-parameter)))

(defun walk-optional-argument (form parent env)
  ;; TODO report bind bug: (bind (((name &optional (default-value nil default-value-supplied?) supplied-p-parameter) (ensure-list form))) )
  (destructuring-bind (name &optional (default-value nil default-value-supplied?) supplied-p-parameter)
      (ensure-list form)
    (with-form-object (arg 'optional-function-argument-form parent
                           :name name
                           :supplied-p-parameter supplied-p-parameter)
      (when default-value-supplied?
        (setf (default-value-of arg) (walk-form default-value :parent arg :environment env))))))

(def unwalker optional-function-argument-form (name supplied-p-parameter)
  (bind ((default-value (awhen (default-value-of -form-)
                          (recurse it))))
    (cond ((and name supplied-p-parameter)
           `(,name ,default-value ,supplied-p-parameter))
          ((and name default-value)
           `(,name ,default-value))
          (name name)
          (t (error "Invalid optional argument")))))

(def (class* e) keyword-function-argument-form (function-argument-form)
  ((keyword-name)
   (default-value nil)
   (supplied-p-parameter)))

(defun effective-keyword-name-of (k)
  (or (keyword-name-of k)
      (intern (symbol-name (name-of k)) :keyword)))

(defun walk-keyword-argument (form parent env)
  (destructuring-bind (name &optional (default-value nil default-value-supplied?) supplied-p-parameter)
      (ensure-list form)
    (let ((name (if (consp name)
                    (second name)
                    name))
          (keyword (if (consp name)
                       (first name)
                       nil)))
      (with-form-object (arg 'keyword-function-argument-form parent
                             :name name
                             :keyword-name keyword
                             :supplied-p-parameter supplied-p-parameter)
        (when default-value-supplied?
          (setf (default-value-of arg) (walk-form default-value :parent arg :environment env)))))))

(def unwalker keyword-function-argument-form (keyword-name name default-value supplied-p-parameter)
  (bind ((default-value (awhen (default-value-of -form-)
                          (recurse it))))
    (cond ((and keyword-name name supplied-p-parameter)
           `((,keyword-name ,name) ,default-value ,supplied-p-parameter))
          ((and name supplied-p-parameter)
           `(,name ,default-value ,supplied-p-parameter))
          ((and name default-value)
           `(,name ,default-value))
          (name name)
          (t (error "Invalid keyword argument")))))

(def (class* e) allow-other-keys-function-argument-form (function-argument-form)
  ())

(def unwalker allow-other-keys-function-argument-form ()
  '&allow-other-keys)

(def (class* e) rest-function-argument-form (function-argument-form)
  ())

(def unwalker rest-function-argument-form (name)
  name)

(def (function e) unwalk-ordinary-lambda-list (arguments)
  (bind ((optional-seen? nil)
         (rest-seen? nil)
         (keyword-seen? nil)
         (auxiliary-seen? nil))
    (mapcan #'(lambda (form)
                (append
                 (etypecase form
                   (required-function-argument-form
                    (assert (not (or optional-seen? rest-seen? keyword-seen? auxiliary-seen?))))
                   (optional-function-argument-form
                    (unless optional-seen?
                      (assert (not (or rest-seen? keyword-seen? auxiliary-seen?)))
                      (setq optional-seen? t)
                      '(&optional)))
                   (rest-function-argument-form
                    (unless rest-seen?
                      (assert (not (or keyword-seen? auxiliary-seen?)))
                      (setq rest-seen? t)
                      '(&rest)))
                   (keyword-function-argument-form
                    (unless keyword-seen?
                      (assert (not auxiliary-seen?))
                      (setq keyword-seen? t)
                      '(&key)))
                   (allow-other-keys-function-argument-form
                    (assert (not auxiliary-seen?)))
                   (auxiliary-function-argument-form
                    (unless auxiliary-seen?
                      (setq auxiliary-seen? t)
                      '(&aux))))
                 (list (unwalk-form form))))
            arguments)))

(def (class* e) auxiliary-function-argument-form (function-argument-form)
  ((default-value nil)))

(defun walk-auxiliary-argument (form parent env)
  (destructuring-bind (name &optional (default-value nil default-value-supplied?))
      (ensure-list form)
    (with-form-object (arg 'auxiliary-function-argument-form parent :name name)
      (when default-value-supplied?
        (setf (default-value-of arg) (walk-form default-value :parent arg :environment env))))))

(def unwalker auxiliary-function-argument-form (name supplied-p-parameter)
  (bind ((default-value (awhen (default-value-of -form-)
                          (recurse it))))
    (cond ((and name default-value)
           `(,name ,default-value))
          (name name)
          (t (error "Invalid auxiliary argument")))))

;;;; FLET/LABELS

(def (class* e) function-binding-form (walked-form binding-form-mixin implicit-progn-with-declare-mixin)
  ())

(def (class* e) flet-form (function-binding-form)
  ())

(def walker flet
  (bind (((bindings &body body) (cdr -form-)))
    (with-form-object (flet 'flet-form -parent-)
      ;; build up the objects for the bindings in the original env
      (loop
         :for entry :in bindings
         :for (name arguments . body) = entry
         :collect (progn
                    (when (< (length entry) 2)
                      (error "Illegal FLET binding form ~S" entry))
                    (cons name (when (or body
                                         arguments)
                                 (with-form-object (lambda-node 'lambda-function-form flet)
                                   (walk-lambda-like lambda-node arguments body -environment-))))) :into bindings
         :finally (setf (bindings-of flet) bindings))
      ;; walk the body in the new env
      (walk-implict-progn flet
                          body
                          (loop
                             :for (name . body) :in (bindings-of flet)
                             :do (-augment- :function name body)
                             :finally (return -environment-))
                          :declare t))))

;; TODO factor out stuff in flet-form and labels-form
(def unwalker flet-form (bindings body declares)
  `(flet ,(mapcar (lambda (binding)
                    (cons (car binding)
                          (if (cdr binding)
                              ;; remove (function (lambda ...)) of the function bindings
                              (rest (second (recurse (cdr binding))))
                              ;; empty args
                              (list '()))))
                  bindings)
     ,@(unwalk-declarations declares)
     ,@(recurse-on-body body)))

(def (class* e) labels-form (function-binding-form)
  ())

(def walker labels
  (bind (((bindings &body body) (cdr -form-)))
    (with-form-object (labels 'labels-form -parent- :bindings '())
      ;; we need to walk over the bindings twice. the first pass
      ;; creates some 'empty' lambda objects in the environment so
      ;; that walked-lexical-application-form and walked-lexical-function-object-form
      ;; have something to point to. the second pass then walks the
      ;; actual bodies of the form filling in the previously created
      ;; objects.
      (loop
         :for entry :in bindings
         :for (name arguments . body) :in bindings
         :for lambda = (when (or body
                            arguments)
                    (with-current-form entry
                      (make-form-object 'lambda-function-form labels)))
         :do (progn
               (when (< (length entry) 2)
                 (error "Illegal LABELS binding form ~S" entry))
               (push (cons name lambda) (bindings-of labels))
               (-augment- :function name lambda)))
      (setf (bindings-of labels) (nreverse (bindings-of labels)))
      (loop
         :for form :in bindings
         :for (arguments . body) = (cdr form)
         :for binding :in (bindings-of labels)
         :for lambda = (cdr binding)
         :do (when lambda
               (let ((tmp-lambda (walk-lambda `(lambda ,arguments ,@body) labels -environment-)))
                 (setf (body-of lambda) (body-of tmp-lambda)
                       (arguments-of lambda) (arguments-of tmp-lambda)
                       (declares-of lambda) (declares-of tmp-lambda)))))
      (walk-implict-progn labels body -environment- :declare t))))

(def unwalker labels-form (bindings body declares)
  `(labels ,(mapcar (lambda (binding)
                      (cons (car binding)
                            (if (cdr binding)
                                ;; remove (function (lambda ...)) of the function bindings
                                (cdadr (recurse (cdr binding)))
                                ;; empty args
                                (list '()))))
                    bindings)
     ,@(unwalk-declarations declares)
     ,@(recurse-on-body body)))
