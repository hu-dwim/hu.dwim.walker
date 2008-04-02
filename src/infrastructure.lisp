;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See COPYING for details.

(in-package :cl-walker)

(defun macroexpand-all (form &optional (env (make-empty-lexenv)))
  (unwalk-form (walk-form form nil (make-walk-environment env))))

(defvar *warn-undefined* nil
  "When non-NIL any references to undefined functions or variables will signal a warning.")

(defun walk-form (form &optional (parent nil) (env (make-walk-environment)))
  "Walk FORM and return a CLOS based AST that represents it."
  (funcall (find-walker-handler form) form parent env))

(defgeneric unwalk-form (form)
  (:documentation "Unwalk FORM and return a list representation."))

(defun unwalk-forms (forms)
  (mapcar #'unwalk-form forms))

(defun register (environment type name datum &rest other-datum)
  (cons (if other-datum
            (list* type name datum other-datum)
            (list* type name datum))
        environment))

(defmacro extend (environment type name datum &rest other-datum)
  `(setf ,environment (register ,environment ,type ,name ,datum ,@other-datum)))

(defun lookup (environment type name &key (error-p nil) (default-value nil))
  (loop
     :for (.type .name . data) :in environment
     :when (and (eql .type type) (eql .name name))
       :return (values data t)
     :finally
       (if error-p
           (error "Sorry, No value for ~S of type ~S in environment ~S found."
                  name type environment)
           (values default-value nil))))

(defun (setf lookup) (value environment type name &key (error-p nil))
  (loop
     for env-piece in environment
     when (and (eql (first env-piece)  type)
               (eql (second env-piece) name))
       do (setf (cddr env-piece) value) and
       return value
     finally
       (when error-p
         (error "Sorry, No value for ~S of type ~S in environment ~S found."
                name type environment))))

(defun make-walk-environment (&optional lexical-env)
  (let ((walk-env '()))
    (when lexical-env
      (do-variables-in-lexenv (lexical-env name ignored?)
        (unless ignored?
          (extend walk-env :unwalked-variable name t)))
      (do-functions-in-lexenv (lexical-env name)
        (extend walk-env :unwalked-function name t))
      (do-macros-in-lexenv (lexical-env name macro-fn)
        (extend walk-env :macro name macro-fn))
      (do-symbol-macros-in-lexenv (lexical-env name definition)
        (extend walk-env :symbol-macro name definition)))
    (cons walk-env lexical-env)))

(defun register-walk-env (env type name datum &rest other-datum)
  (declare (ignore other-datum)) ;; TODO ?
  (let ((walk-env (register (car env) type name datum))
        (lexenv (cdr env)))
    (cons walk-env (ecase type
                     (:variable     (augment-lexenv-with-variable name lexenv))
                     (:macro        (augment-lexenv-with-macro name datum lexenv))
                     (:function     (augment-lexenv-with-function name lexenv))
                     (:symbol-macro (augment-lexenv-with-symbol-macro name datum lexenv))
                     ;; TODO
                     (:declare      lexenv)
                     (:block        lexenv)
                     (:tagbody      lexenv)
                     (:tag          lexenv)))))

(defmacro extend-walk-env (env type name datum &rest other-datum)
  `(setf ,env (register-walk-env ,env ,type ,name ,datum ,@other-datum)))

(defun lookup-walk-env (env type name &key (error-p nil) (default-value nil))
  (lookup (car env) type name :error-p error-p :default-value default-value))

(defparameter *walker-handlers* (make-hash-table :test 'eq))

(define-condition undefined-reference (warning)
  ((enclosing-code :accessor enclosing-code-of :initform nil)
   (name :accessor name-of :initarg :name)))

(define-condition undefined-variable-reference (undefined-reference)
  ()
  (:report
   (lambda (c s)
     (if (enclosing-code-of c)
         (format s "Reference to unknown variable ~S in ~S." (name-of c) (enclosing-code-of c))
         (format s "Reference to unknown variable ~S." (name-of c))))))

(define-condition undefined-function-reference (undefined-reference)
  ()
  (:report
   (lambda (c s)
     (if (enclosing-code-of c)
         (format s "Reference to unknown function ~S in ~S." (name-of c) (enclosing-code-of c))
         (format s "Reference to unknown function ~S." (name-of c))))))

(defparameter +atom-marker+ '+atom-marker+)

(defun find-walker-handler (form)
  "Simple function which tells us what handler should deal
  with FORM. Signals an error if we don't have a handler for
  FORM."
  (if (atom form)
      (gethash '+atom-marker+ *walker-handlers*)
      (aif (gethash (car form) *walker-handlers*)
           it
           (case (car form)
             ((block declare flet function go if labels let let*
                     macrolet progn quote return-from setq symbol-macrolet
                     tagbody unwind-protect catch multiple-value-call
                     multiple-value-prog1 throw load-time-value the
                     eval-when locally progv)
              (error "Sorry, no walker for the special operator ~S defined." (car form)))
             (t (gethash 'application *walker-handlers*))))))

(defmacro defwalker-handler (name (form parent lexical-env)
                             &body body)
  `(progn
     (setf (gethash ',name *walker-handlers*)
           (lambda (,form ,parent ,lexical-env)
             (declare (ignorable ,parent ,lexical-env))
             ,@body))
     ',name))

(defmacro defunwalker-handler (class (&rest slots) &body body)
  (with-unique-names (form)
    `(progn
       (defmethod unwalk-form ((,form ,class))
         (with-slots ,slots ,form
           ,@body))
       ',class)))

(defclass form ()
  ((parent :accessor parent :initarg :parent)
   (source :accessor source :initarg :source)))

(defmethod make-load-form ((object form) &optional env)
  (make-load-form-saving-slots object :environment env))

(defmethod print-object ((form form) stream)
  (print-unreadable-object (form stream :type t :identity t)
    (when (slot-boundp form 'source)
      (let ((*print-readably* nil)
            (*print-level* 0)
            (*print-length* 4))
        (format stream "~S" (source form))))))

(defmacro with-form-object ((variable type &rest initargs)
                            &body body)
  `(let ((,variable (make-instance ',type ,@initargs)))
     ,@body
     ,variable))

(defclass implicit-progn-mixin ()
  ((body :accessor body :initarg :body)))

(defclass implicit-progn-with-declare-mixin (implicit-progn-mixin)
  ((declares :accessor declares :initarg :declares)))

(defclass binding-form-mixin ()
  ((bindings :accessor bindings-of :initarg :bindings)))

(defmacro multiple-value-setf (places form)
  `(let (_)
     (declare (ignorable _))
     (setf (values ,@(mapcar (lambda (el)
                               (if (and (symbolp el)
                                        (or (eq el nil)
                                            (string= el '_)))
                                   '_
                                   el))
                             places))
           ,form)))

(defun split-body (body env &key parent (docstring t) (declare t))
  (let ((documentation nil)
        (newdecls nil)
        (decls nil))
    (flet ((done ()
             (return-from split-body (values body env documentation (nreverse decls)))))
      (loop
         for form = (car body)
         while body
         do (typecase form
              (cons (if (and declare (eql 'cl:declare (first form)))
                        ;; declare form
                        (let ((declarations (rest form)))
                          (dolist (dec declarations)
                            (setf (values env newdecls) (parse-declaration dec env parent))
                            (setf decls (append newdecls decls))))
                        ;; source code, all done
                        (done)))
              (string (if docstring
                          (if documentation
                              ;; already found the docstring, this is source
                              (done)
                              (if (cdr body)
                                  ;; found the doc string
                                  (setf documentation form)
                                  ;; this looks like a doc string, but
                                  ;; it's the only form in body, so
                                  ;; it's actually code.
                                  (done)))
                          ;; no docstring allowed, this is source
                          (done)))
              (t ;; more code, all done
               (done)))
         do (pop body)
         finally (done)))))

(defun parse-macro-definition (name lambda-list body env)
  "Sort of like parse-macro from CLtL2."
  (declare (ignore name))
  (let* ((environment-var nil)
         (lambda-list-without-environment
          (loop
           for prev = nil then i
           for i in lambda-list
           if (not (or (eq '&environment i) (eq '&environment prev)))
           collect i
           if (eq '&environment prev)
           do (if (eq environment-var nil)
                  (setq environment-var i)
                  (error "Multiple &ENVIRONMENT clauses in macro lambda list: ~S" lambda-list))))
         (handler-env (if (eq environment-var nil) (gensym "ENV-") environment-var))
         whole-list lambda-list-without-whole)
    (if (eq '&whole (car lambda-list-without-environment))
        (setq whole-list (list '&whole (second lambda-list-without-environment))
              lambda-list-without-whole (cddr lambda-list-without-environment))
        (setq whole-list '()
              lambda-list-without-whole lambda-list-without-environment))
    (eval
     (with-unique-names (handler-args form-name)
       `(lambda (,handler-args ,handler-env)
          ,@(unless environment-var
              `((declare (ignore ,handler-env))))
          (destructuring-bind (,@whole-list ,form-name ,@lambda-list-without-whole)
              ,handler-args
            (declare (ignore ,form-name))
            ,@(mapcar (lambda (form)
                        (macroexpand-all form env))
                      body)))))))
