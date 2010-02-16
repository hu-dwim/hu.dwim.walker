;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.walker)

(def form-class application-form ()
  ((operator :ast-link t)
   (arguments :ast-link t)))

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

(def form-class lexical-application-form (application-form)
  ((definition :ast-link :back)))

(def form-class walked-lexical-application-form (lexical-application-form)
  ())

(def form-class unwalked-lexical-application-form (lexical-application-form)
  ())

(def form-class free-application-form (application-form)
  ())

(def form-class lambda-application-form (application-form)
  ())

(def unwalker lambda-application-form (operator arguments)
  ;; KLUDGE: The cadr is for getting rid of (function ...) which we can't have at the beginning of a form.
  (cons (cadr (recurse operator)) (recurse-on-body arguments)))

(def layered-method walk-form/application (-form- -parent- operator arguments -environment-)
  (macrolet ((-lookup- (type name &key (otherwise nil))
               `(%repository/find (env/walked-environment -environment-) ,type ,name :otherwise ,otherwise)))
    (labels ((recurse (node &optional (parent -parent-))
               (walk-form node :parent parent :environment -environment-))
             (walk-arguments (application-form)
               (loop
                 :for index :from 1
                 :for arg :in arguments
                 :collect (recurse arg application-form))))
      (when (lambda-form? operator)
        (return-from walk-form/application
          (with-form-object (application 'lambda-application-form -parent-)
            (setf (operator-of application) (walk-form/lambda operator application -environment-)
                  (arguments-of application) (walk-arguments application)))))
      (bind ((lexenv (env/lexical-environment -environment-))
             ((:values innermost-lexical-definition-type nil expander) (-lookup- nil operator)))
        (awhen (eq :macro innermost-lexical-definition-type)
          (bind ((*inside-macroexpansion* t)
                 (expansion (funcall expander -form- lexenv)))
            (return-from walk-form/application (recurse expansion))))
        (when (and (symbolp (coerce-to-form operator))
                   (macro-name? (coerce-to-form operator) lexenv)
                   (not (member innermost-lexical-definition-type '(:function :unwalked-function))))
          (bind (((:values expansion expanded?) (walker-macroexpand-1 -form- lexenv)))
            (when expanded?
              (bind ((*inside-macroexpansion* t))
                (return-from walk-form/application (recurse expansion)))))))
      (bind ((application-form (aif (-lookup- :function operator)
                                    (make-instance 'walked-lexical-application-form :definition it)
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

(def form-class function-form ()
  ())

(def form-class lambda-function-form (function-form implicit-progn-with-declarations-mixin)
  ((arguments :ast-link t)))

(def unwalker lambda-function-form (arguments body declarations)
  `#'(lambda ,(unwalk-ordinary-lambda-list arguments)
       ,@(unwalk-declarations declarations)
       ,@(recurse-on-body body)))

(def form-class function-definition-form (lambda-function-form
                                          name-definition-form)
  ((docstring nil)))

(def walker defun
  (bind (((name args &rest body) (rest -form-)))
    (with-form-object (node 'function-definition-form -parent- :name name)
      (walk-form/lambda-like node args body
                             (-augment- :function name node)
                             :docstring-allowed t :declarations-allowed t))))

(def unwalker function-definition-form (form name arguments body docstring declarations)
  `(defun ,name ,(unwalk-ordinary-lambda-list arguments)
     ,@(when docstring (list docstring))
     ,@(unwalk-declarations declarations)
     ,@(recurse-on-body body)))

(def form-class named-lambda-function-form (lambda-function-form
                                            name-definition-form)
  ((special-form)))

(def unwalker named-lambda-function-form (special-form name arguments body declarations)
  `(function
    (,special-form ,name ,(unwalk-ordinary-lambda-list arguments)
     ,@(unwalk-declarations declarations)
     ,@(recurse-on-body body))))

(def form-class lexical-function-form (lambda-function-form
                                       name-definition-form)
  ())

(def unwalker lexical-function-form (name arguments body declarations)
  `(,name ,(unwalk-ordinary-lambda-list arguments)
     ,@(unwalk-declarations declarations)
     ,@(recurse-on-body body)))

(def form-class function-object-form (named-walked-form)
  ())

(def unwalker function-object-form (name)
  `(function ,name))

(def form-class lexical-function-object-form (function-object-form)
  ())

(def form-class walked-lexical-function-object-form (lexical-function-object-form)
  ((definition :ast-link :back)))

(def form-class unwalked-lexical-function-object-form (lexical-function-object-form)
  ())

(def form-class free-function-object-form (function-object-form)
  ())

(def walker function
  (cond
    ((lambda-form? (second -form-))
     ;; specially handling (function (lambda ...))
     (walk-form/lambda (coerce-to-form (second -form-)) -parent- -environment-))
    #+sbcl
    ((and (consp (second -form-))
          (eq (first (second -form-)) 'sb-int:named-lambda))
     (bind ((named-lambda-form (second -form-)))
       (with-form-object (node 'named-lambda-function-form -parent-
                               :special-form (first named-lambda-form)
                               :name (second named-lambda-form))
         (walk-form/lambda-like node (third named-lambda-form)
                                (nthcdr 3 named-lambda-form)
                                (-augment- :function (name-of node) node)
                                :declarations-allowed t))))
    (t
     ;; (function foo)
     (bind ((function-name (second -form-))
            (walked-lexical-function (-lookup- :function function-name)))
       (if walked-lexical-function
           (make-form-object 'walked-lexical-function-object-form -parent-
                             :name function-name
                             :definition walked-lexical-function)
           (make-form-object (if (-lookup- :unwalked-function function-name)
                                 'unwalked-lexical-function-object-form
                                 'free-function-object-form)
                             -parent-
                             :name function-name))))))

(def layered-method walk-form/lambda (form parent env)
  (assert (string-equal (coerce-to-form (first form)) "lambda")) ;; because the js walker comes in with '|lambda|...
  (with-form-object (ast-node 'lambda-function-form parent)
    (walk-form/lambda-like ast-node (second form) (cddr form) env :declarations-allowed t)))

(def layered-methods walk-form/lambda-like
  (:method :before (ast-node args body env &key &allow-other-keys)
    (check-type env walk-environment))
  (:method (ast-node args body env &key docstring-allowed declarations-allowed (whole *current-form*))
    (setf (values (arguments-of ast-node) env) (walk-ordinary-lambda-list (coerce-to-form args) ast-node env))
    (walk-implict-progn ast-node body env
                        :docstring-allowed docstring-allowed :declarations-allowed declarations-allowed
                        :whole whole)
    ast-node))

(def (function e) walk-ordinary-lambda-list (lambda-list parent env &key allow-specializers)
  (bind (((:values requireds optionals rest keywords allow-other-keys? auxiliaries) (parse-ordinary-lambda-list (mapcar 'coerce-to-form lambda-list) :normalize nil))
         (args (nconc
                (loop
                  :for required :in requireds
                  :collect (bind ((arg (if allow-specializers
                                           (make-form-object 'specialized-function-argument-form parent
                                                             :name (first (ensure-list required))
                                                             :specializer (or (second (ensure-list required)) t))
                                           (make-form-object 'required-function-argument-form parent
                                                             :name required))))
                             (augment-walk-environment! env :variable (name-of arg) arg)
                             arg))
                (loop
                  :for optional :in optionals
                  ;; TODO report bind bug: (bind (((name &optional (default-value nil default-value-supplied?) supplied-p-parameter) (ensure-list form))) )
                  ;; it blingly replaces all nil's with '#:BIND-IGNORE-1305, including the nil default value above.
                  :collect (destructuring-bind (name &optional (default-value nil default-value-supplied?) supplied-p-parameter-name)
                               (ensure-list optional)
                             (with-form-object (arg 'optional-function-argument-form parent
                                                    :name name
                                                    :supplied-p-parameter-name supplied-p-parameter-name)
                               (augment-walk-environment! env :variable name arg)
                               (when default-value-supplied?
                                 (setf (default-value-of arg) (walk-form default-value :parent arg :environment env)))
                               (when supplied-p-parameter-name
                                 ;; TODO so, what on earth do we want to store for supplied-p-parameter-name? it should be a full lexical-variable-binding-form so that ...
                                 (augment-walk-environment! env :variable supplied-p-parameter-name t)))))
                (when rest
                  (bind ((arg (make-form-object 'rest-function-argument-form parent :name rest)))
                    (augment-walk-environment! env :variable rest arg)
                    (list arg)))
                (loop
                  :for keyword :in keywords
                  :collect (destructuring-bind (name &optional (default-value nil default-value-supplied?) supplied-p-parameter-name)
                               (ensure-list keyword)
                             (bind ((name (if (consp name) (second name) name))
                                    (keyword (if (consp name) (first name) nil)))
                               (with-form-object (arg 'keyword-function-argument-form parent
                                                      :name name
                                                      :keyword-name keyword
                                                      :supplied-p-parameter-name supplied-p-parameter-name)
                                 (augment-walk-environment! env :variable name arg)
                                 (when default-value-supplied?
                                   (setf (default-value-of arg) (walk-form default-value :parent arg :environment env)))
                                 (when supplied-p-parameter-name
                                   ;; TODO see similar comment at &optional
                                   (augment-walk-environment! env :variable supplied-p-parameter-name t))))))
                (when allow-other-keys?
                  (list (make-form-object 'allow-other-keys-function-argument-form parent)))
                (loop
                  :for auxiliary :in auxiliaries
                  :collect (destructuring-bind (name &optional (default-value nil default-value-supplied?))
                               (ensure-list auxiliary)
                             (with-form-object (arg 'auxiliary-function-argument-form parent :name name)
                               (augment-walk-environment! env :variable name arg)
                               (when default-value-supplied?
                                 (setf (default-value-of arg) (walk-form default-value :parent arg :environment env)))))))))
    (values args env)))

(def form-class function-argument-form (name-definition-form)
  ())

(def print-object function-argument-form
  (format t "~S" (name-of -self-)))

(def form-class required-function-argument-form (function-argument-form)
  ())

(def unwalker required-function-argument-form (name)
  name)

(def form-class specialized-function-argument-form (required-function-argument-form)
  ((specializer)))

(def unwalker specialized-function-argument-form (name specializer)
  (if (eq specializer t)
      name
      `(,name ,specializer)))

(def form-class function-argument-form-with-default-value (function-argument-form)
  ((default-value nil :ast-link t)))

(def form-class function-argument-form-with-supplied-p-parameter (function-argument-form-with-default-value)
  ((supplied-p-parameter-name)))

(def form-class optional-function-argument-form (function-argument-form-with-supplied-p-parameter)
  ())

(def unwalker optional-function-argument-form (name supplied-p-parameter-name)
  (bind ((default-value (awhen (default-value-of -form-)
                          (recurse it))))
    (cond ((and name supplied-p-parameter-name)
           `(,name ,default-value ,supplied-p-parameter-name))
          ((and name default-value)
           `(,name ,default-value))
          (name name)
          (t (error "Invalid optional argument")))))

(def form-class keyword-function-argument-form (function-argument-form-with-supplied-p-parameter)
  ((keyword-name)))

(def (function e) effective-keyword-name-of (k)
  (or (keyword-name-of k)
      (intern (symbol-name (name-of k)) :keyword)))

(def unwalker keyword-function-argument-form (keyword-name name default-value supplied-p-parameter-name)
  (bind ((default-value (awhen (default-value-of -form-)
                          (recurse it))))
    (cond ((and keyword-name name supplied-p-parameter-name)
           `((,keyword-name ,name) ,default-value ,supplied-p-parameter-name))
          ((and name supplied-p-parameter-name)
           `(,name ,default-value ,supplied-p-parameter-name))
          ((and name default-value)
           `(,name ,default-value))
          (name name)
          (t (error "Invalid keyword argument")))))

(def form-class allow-other-keys-function-argument-form (function-argument-form)
  ())

(def unwalker allow-other-keys-function-argument-form ()
  '&allow-other-keys)

(def form-class rest-function-argument-form (function-argument-form)
  ())

(def unwalker rest-function-argument-form (name)
  name)

(def form-class auxiliary-function-argument-form (function-argument-form-with-default-value)
  ())

(def unwalker auxiliary-function-argument-form (name supplied-p-parameter)
  (bind ((default-value (awhen (default-value-of -form-)
                          (recurse it))))
    (cond ((and name default-value)
           `(,name ,default-value))
          (name name)
          (t (error "Invalid auxiliary argument")))))

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

;;;; FLET/LABELS

(def form-class function-binding-form (binder-form-mixin
                                       implicit-progn-with-declarations-mixin)
  ())

(def form-class flet-form (function-binding-form)
  ())

(def walker flet
  (bind (((bindings &body body) (cdr -form-)))
    (with-form-object (flet 'flet-form -parent-)
      ;; build up the objects for the bindings in the original env
      (setf (bindings-of flet)
            (loop
              :for entry :in bindings
              :for (name arguments . body) = entry
              :collect (progn
                         (when (< (length entry) 2)
                           (error "Illegal FLET binding form ~S" entry))
                         (with-current-form entry
                           (with-form-object (lambda-node 'lexical-function-form flet :name name)
                             (walk-form/lambda-like lambda-node arguments body -environment- :declarations-allowed t))))))
      ;; augment the walkenv with the new flet bindings
      (loop
         :for definition :in (bindings-of flet)
         :do (-augment- :function (name-of definition) definition))
      ;; walk the body in the new env
      (walk-implict-progn flet body -environment- :declarations-allowed t))))

(def function unwalk-flet-or-labels (name bindings body declarations)
  `(,name ,(mapcar #'unwalk-form bindings)
     ,@(unwalk-declarations declarations)
     ,@(mapcar 'unwalk-form body)))

(def unwalker flet-form (bindings body declarations)
  (unwalk-flet-or-labels 'flet bindings body declarations))

(def form-class labels-form (function-binding-form)
  ())

(def walker labels
  (bind (((bindings &body body) (cdr -form-)))
    (with-form-object (labels 'labels-form -parent- :bindings '())
      ;; we need to walk over the bindings twice. the first pass creates some 'empty' lambda objects in the environment so
      ;; that WALKED-LEXICAL-APPLICATION-FORM and WALKED-LEXICAL-FUNCTION-OBJECT-FORM have something to point to.
      ;; the second pass then walks the actual bodies of the form filling in the previously created objects.
      (loop
        :for entry :in bindings
        :for (name arguments . body) :in bindings
        :do (bind ((definition (with-current-form entry
                                 (make-form-object 'lexical-function-form labels :name name))))
              (when (< (length entry) 2)
                (error "Illegal LABELS binding form ~S" entry))
              (push definition (bindings-of labels))
              ;; augment walkenv with the not-yet-walked definition for the upcoming entries
              (-augment- :function name definition)))
      (setf (bindings-of labels) (nreverse (bindings-of labels)))
      (loop
         :for (name arguments . body) :in bindings
         :for definition :in (bindings-of labels)
         :do (progn
               (assert (eq name (name-of definition)))
               (walk-form/lambda-like definition arguments body -environment-)))
      (walk-implict-progn labels body -environment- :declarations-allowed t))))

(def unwalker labels-form (bindings body declarations)
  (unwalk-flet-or-labels 'labels bindings body declarations))
