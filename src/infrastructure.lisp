;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See COPYING for details.

(in-package :cl-walker)

(define-condition walker-error (error)
  ())

(define-condition simple-walker-error (simple-error walker-error)
  ())

(defun simple-walker-error (message &rest args)
  (error 'simple-walker-error :format-control message :format-arguments args))

(defun macroexpand-all (form &optional (env (make-empty-lexical-environment)))
  (unwalk-form (walk-form form nil (make-walk-environment env))))

(defvar *warn-for-undefined-references* t
  "When non-NIL, any references to undefined functions or variables will signal a warning.")

(defun walk-form (form &optional (parent nil) (env (make-walk-environment)))
  "Walk FORM and return a CLOS based AST that represents it."
  (funcall (find-walker-handler* form) form parent env))

(defgeneric unwalk-form (form)
  (:documentation "Unwalk FORM and return a list representation."))

(defun unwalk-forms (forms)
  (mapcar #'unwalk-form forms))

(defun special-variable-name? (name)
  (or (boundp name)
      #+sbcl(eq (sb-int:info :variable :kind name) :special)
      #+lispworks(eq (cl::variable-information name) :special)))

(defvar *walker-context*)

#+nil
(defclass-star:defclass* walker-context ()
  ((find-walker-handler         (find-walker-handler-of *walker-context*))
   (function-name?              (function-name?-of *walker-context*))
   (macro-name?                 (macro-name?-of *walker-context*))
   (macroexpand-1               (macroexpand-1-of *walker-context*))
   (symbol-macro-name?          (symbol-macro-name?-of *walker-context*))
   (constant-name?              (constant-name?-of *walker-context*))
   (lambda-form?                (lambda-form?-of *walker-context*))
   (undefined-reference-handler (undefined-reference-handler-of *walker-context*))))

;; macroexpansion of the above defclass*
(defclass walker-context ()
  ((find-walker-handler :initform (find-walker-handler-of *walker-context*) :accessor find-walker-handler-of :initarg :find-walker-handler)
   (function-name? :initform (function-name?-of *walker-context*) :accessor function-name?-of :initarg :function-name?)
   (macro-name? :initform (macro-name?-of *walker-context*) :accessor macro-name?-of :initarg :macro-name?)
   (macroexpand-1 :initform (macroexpand-1-of *walker-context*) :accessor macroexpand-1-of :initarg :macroexpand-1)
   (symbol-macro-name? :initform (symbol-macro-name?-of *walker-context*) :accessor symbol-macro-name?-of :initarg :symbol-macro-name?)
   (constant-name? :initform (constant-name?-of *walker-context*) :accessor constant-name?-of :initarg :constant-name?)
   (lambda-form? :initform (lambda-form?-of *walker-context*) :accessor lambda-form?-of :initarg :lambda-form?)
   (undefined-reference-handler :initform (undefined-reference-handler-of *walker-context*) :accessor undefined-reference-handler-of :initarg
                                :undefined-reference-handler)))

(setf *walker-context* (make-instance 'walker-context
                                      :find-walker-handler         'find-walker-handler
                                      :function-name?              '%function-name?
                                      :macro-name?                 'macro-function
                                      :macroexpand-1               'macroexpand-1
                                      :symbol-macro-name?          '%symbol-macro-name?
                                      :constant-name?              '%constant-name?
                                      :lambda-form?                '%lambda-form?
                                      :undefined-reference-handler 'undefined-reference-handler))

(defun find-walker-handler* (name)
  (funcall (find-walker-handler-of *walker-context*) name))

(defun function-name? (name)
  (funcall (function-name?-of *walker-context*) name))

(defun %function-name? (name)
  (or #+sbcl(eq (sb-int:info :function :kind name) :function)
      (fboundp name)))

(defun macro-name? (name &optional env)
  (funcall (macro-name?-of *walker-context*) name env))

(defun symbol-macro-name? (name &optional env)
  (funcall (symbol-macro-name?-of *walker-context*) name env))

(defun %symbol-macro-name? (name &optional env)
  (nth-value 1 (macroexpand-1 name env)))

(defun constant-name? (name &optional env)
  (funcall (constant-name?-of *walker-context*) name env))

(defun %constant-name? (form &optional env)
  (declare (ignore env))
  (or (eq form t)
      (eq form nil)
      (not (or (symbolp form)
               (consp form)))))

(defun lambda-form? (form &optional env)
  (funcall (lambda-form?-of *walker-context*) form env))

(defun %lambda-form? (form &optional env)
  (declare (ignore env))
  (and (consp form)
       (eq 'cl:lambda (car form))))

(defun walker-macroexpand-1 (form &optional env)
  (funcall (macroexpand-1-of *walker-context*) form env))

(defun undefined-reference-handler (type name)
  (ecase type
    (:function (warn 'undefined-function-reference :name name))
    (:variable (warn 'undefined-variable-reference :name name))))

(defun undefined-reference (type name)
  (funcall (undefined-reference-handler-of *walker-context*) type name))

(defmacro with-walker-configuration ((&rest args) &body body)
  "See the WALKER-CONTEXT class for possible arguments."
  `(let ((*walker-context* (make-instance 'walker-context ,@args)))
     ,@body))

;;;
;;; Walk environment
;;;

;; there are three players here:
;; 1) the walkenv, which contains the already walked *-form instances
;; 2) the lexenv, which is the underlying lisp's internal lexenv
;; 3) the combined environment, which is (cons walkenv lexenv)
;;
;; %lookup and friends are internal utils to update/query the walkenv.

(defun make-walk-environment (&optional lexenv)
  (unless lexenv
    (setf lexenv (make-empty-lexical-environment)))
  (let ((walkenv '()))
    (macrolet ((extend! (environment type name datum &rest other-datum)
                 `(setf ,environment (%extend ,environment ,type ,name ,datum ,@other-datum))))
      (do-variables-in-lexenv (lexenv name ignored?)
        (unless ignored?
          (extend! walkenv :unwalked-variable name t)))
      (do-functions-in-lexenv (lexenv name)
        (extend! walkenv :unwalked-function name t))
      (do-macros-in-lexenv (lexenv name macro-fn)
        (extend! walkenv :macro name macro-fn))
      (do-symbol-macros-in-lexenv (lexenv name definition)
        (extend! walkenv :symbol-macro name definition)))
    (cons walkenv lexenv)))

(defun augment-walkenv (env type name datum)
  (let ((walkenv (%extend (car env) type name datum))
        (lexenv (cdr env)))
    (cons walkenv (ecase type
                    (:variable     (augment-lexenv-with-variable name lexenv))
                    (:macro        (augment-lexenv-with-macro name datum lexenv))
                    (:function     (augment-lexenv-with-function name lexenv))
                    (:symbol-macro (augment-lexenv-with-symbol-macro name datum lexenv))
                    (:block        (augment-lexenv-with-block name lexenv))
                    (:tag          (augment-lexenv-with-tag name lexenv))
                    ;; TODO
                    (:declare      lexenv)
                    (:tagbody      lexenv)))))

(defmacro augment-walkenv! (env type name datum &rest other-datum)
  `(setf ,env (augment-walkenv ,env ,type ,name ,datum ,@other-datum)))

(defun lookup-in-walkenv (type name env &key (error-p nil) (default-value nil))
  (%lookup (car env) type name :error-p error-p :default-value default-value))

(defun %extend (environment type name datum &rest other-datum)
  (cons (if other-datum
            (list* type name datum other-datum)
            (list* type name datum))
        environment))

(defun %lookup (environment type name &key (error-p nil) (default-value nil))
  (loop
     :for (.type .name . data) :in environment
     :when (and (eql .type type) (eql .name name))
       :return (values data t)
     :finally
       (if error-p
           (error "No value for ~S of type ~S in environment ~S was found."
                  name type environment)
           (values default-value nil))))

#+(or) ;; it's not used for now
(defun (setf %lookup) (value environment type name &key (error-p nil))
  (loop
     :for env-piece :in environment
     :when (and (eql (first env-piece) type)
                (eql (second env-piece) name))
       :do (progn
             (setf (cddr env-piece) value)
             (return value))
     :finally
       (when error-p
         (error "No value for ~S of type ~S in environment ~S was found."
                name type environment))))

;;;
;;; Handler management
;;;

(defparameter *walker-handlers* (make-hash-table :test 'eq))

(define-condition undefined-reference (style-warning)
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

(defgeneric find-walker-handler (form)
  (:documentation "Simple function which tells us what handler should deal with FORM. Signals an error if we don't have a handler for FORM.")
  (:method ((form cons))
    (aif (gethash (car form) *walker-handlers*)
         it
         (case (car form)
           ((block declare flet function go if labels let let*
                   macrolet progn quote return-from setq symbol-macrolet
                   tagbody unwind-protect catch multiple-value-call
                   multiple-value-prog1 throw load-time-value the
                   eval-when locally progv)
            (error "Sorry, no walker for the special operator ~S defined." (car form)))
           (t (gethash 'application *walker-handlers*)))))
  (:method ((form t))
    (gethash '+atom-marker+ *walker-handlers*)))

(defun walker-handler-definition (name &optional (table *walker-handlers*))
  (gethash name table))

(defun (setf walker-handler-definition) (handler name &optional (table *walker-handlers*))
  (when (gethash name table)
    (simple-style-warning "Redefining walker handler for ~S" name))
  (setf (gethash name table) handler))

(defmacro defwalker-handler (name (form parent lexenv)
                             &body body)
  `(progn
     (setf (walker-handler-definition ',name)
           (named-lambda ,(format-symbol *package* "WALKER-HANDLER/~A" name)
               (,form ,parent ,lexenv)
             (declare (ignorable ,parent ,lexenv))
             ,@body))
     ',name))

(defmacro defwalker-handler-alias (from-name to-name)
  `(progn
     (setf (walker-handler-definition ',to-name) (walker-handler-definition ',from-name))
     ',to-name))

(defmacro defunwalker-handler (class (&rest slots) &body body)
  `(progn
     (defmethod unwalk-form ((-form- ,class))
       (with-slots ,slots -form-
         ,@body))
     ',class))

(defclass walked-form ()
  ((parent :accessor parent-of :initarg :parent)
   (source :accessor source-of :initarg :source)))

(defmethod make-load-form ((object walked-form) &optional env)
  (make-load-form-saving-slots object :environment env))

(defmethod print-object ((form walked-form) stream)
  (print-unreadable-object (form stream :type t :identity t)
    (when (slot-boundp form 'source)
      (let ((*print-readably* nil)
            (*print-level* 0)
            (*print-length* 4))
        (format stream "~S" (source-of form))))))

(defmacro with-form-object ((variable type &rest initargs)
                            &body body)
  `(let ((,variable (make-instance ',type ,@initargs)))
     ,@body
     ,variable))

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
  (let ((walked-declarations (list)))
    (multiple-value-bind (body declarations documentation)
        (parse-body body :documentation docstring)
      (when declarations
        (unless declare
          (error "Declarations are not allowed at ~S" body))
        (dolist (declaration declarations)
          (assert (eq (first declaration) 'declare))
          (dolist (entry (rest declaration))
            (let ((newdecls nil))
              (setf (values env newdecls) (walk-declaration entry env parent))
              (appendf walked-declarations newdecls)))))
      (values body env documentation walked-declarations))))

(defun parse-macro-definition (name lambda-list body &optional lexenv)
  "Sort of like parse-macro from CLtL2."
  (declare (ignore name))
  ;; TODO could use parse-lambda-list
  (let* ((environment-var nil)
         (lambda-list-without-environment
          (loop
             :for prev = nil :then i
             :for i :in lambda-list
             :when (not (or (eq '&environment i)
                            (eq '&environment prev)))
               :collect i
             :when (eq '&environment prev)
               :do (if (eq environment-var nil)
                       (setq environment-var i)
                       (error "Multiple &ENVIRONMENT clauses in macro lambda list: ~S" lambda-list))))
         (handler-env (if (eq environment-var nil) (gensym "ENV-") environment-var))
         whole-list
         lambda-list-without-whole)
    (if (eq '&whole (car lambda-list-without-environment))
        (setq whole-list (list '&whole (second lambda-list-without-environment))
              lambda-list-without-whole (cddr lambda-list-without-environment))
        (setq whole-list '()
              lambda-list-without-whole lambda-list-without-environment))
    (eval
     (with-unique-names (handler-args form-name)
       `(lambda (,handler-args &optional ,handler-env)
          ,@(unless environment-var
              `((declare (ignore ,handler-env))))
          (destructuring-bind (,@whole-list ,form-name ,@lambda-list-without-whole)
              ,handler-args
            (declare (ignore ,form-name))
            ,@(progn
               (when lexenv
                 (dolist (variable (lambda-list-to-variable-name-list
                                    lambda-list-without-whole :macro t :include-specials t))
                   ;; augment the lexenv with the macro's variables, so
                   ;; that we don't get free variable warnings while
                   ;; walking the body of the macro.
                   (augment-lexenv! :variable variable lexenv)))
               (mapcar (lambda (form)
                         (macroexpand-all form lexenv))
                       body))))))))
