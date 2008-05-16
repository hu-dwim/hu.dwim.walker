;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See COPYING for details.

(in-package :cl-walker)

(defun macroexpand-all (form &optional (env (make-empty-lexical-environment)))
  (unwalk-form (walk-form form nil (make-walk-environment env))))

(defvar *warn-for-undefined-references* t
  "When non-NIL, any references to undefined functions or variables will signal a warning.")

(defun walk-form (form &optional (parent nil) (env (make-walk-environment)))
  "Walk FORM and return a CLOS based AST that represents it."
  (funcall (find-walker-handler form) form parent env))

(defgeneric unwalk-form (form)
  (:documentation "Unwalk FORM and return a list representation."))

(defun unwalk-forms (forms)
  (mapcar #'unwalk-form forms))

(defun special-variable-name? (name)
  (or (boundp name)
      #+sbcl(eq (sb-int:info :variable :kind name) :special)
      #+lispworks(eq (cl::variable-information name) :special)))

(defparameter *function-name?*     '%function-name?)
(defparameter *macro-name?*        'macro-function)
(defparameter *macroexpand-1*      'macroexpand-1)
(defparameter *symbol-macro-name?* '%symbol-macro-name?)
(defparameter *constant-name?*     '%constant-name?)
(defparameter *undefined-reference-handler* 'undefined-reference-handler)

(defun function-name? (name)
  (funcall *function-name?* name))

(defun %function-name? (name)
  (or #+sbcl(eq (sb-int:info :function :kind name) :function)
      (fboundp name)))

(defun macro-name? (name &optional env)
  (funcall *macro-name?* name env))

(defun symbol-macro-name? (name &optional env)
  (funcall *symbol-macro-name?* name env))

(defun %symbol-macro-name? (name &optional env)
  (nth-value 1 (macroexpand-1 name env)))

(defun constant-name? (name &optional env)
  (funcall *constant-name?* name env))

(defun %constant-name? (form &optional env)
  (declare (ignore env))
  (or (eq form t)
      (eq form nil)
      (not (or (symbolp form)
               (consp form)))))

(defun walker-macroexpand-1 (form &optional env)
  (funcall *macroexpand-1* form env))

(defun undefined-reference-handler (type name)
  (ecase type
    (:function (warn 'undefined-function-reference :name name))
    (:variable (warn 'undefined-variable-reference :name name))))

(defmacro with-walker-configuration ((&key (function-name? '*function-name?*)
                                           (macro-name? '*macro-name?*)
                                           (symbol-macro-name? '*symbol-macro-name?*)
                                           (constant-name? '*constant-name?*)
                                           (macroexpand-1 '*macroexpand-1*)
                                           (warn-for-undefined-references '*warn-for-undefined-references*)
                                           (undefined-reference-handler '*undefined-reference-handler*)
                                           (handlers '*walker-handlers*))
                                     &body body)
  `(let ((*warn-for-undefined-references* ,warn-for-undefined-references)
         (*undefined-reference-handler* ,undefined-reference-handler)
         (*function-name?* ,function-name?)
         (*macro-name?* ,macro-name?)
         (*symbol-macro-name?* ,symbol-macro-name?)
         (*constant-name?* ,constant-name?)
         (*macroexpand-1* ,macroexpand-1)
         (*walker-handlers* ,handlers))
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

(defun copy-walker-handlers ()
  (copy-hash-table *walker-handlers*))

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

(defun find-walker-handler (form)
  "Simple function which tells us what handler should deal with FORM. Signals an error if we don't have a handler for FORM."
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

(defmacro defwalker-handler (name (form parent lexenv)
                             &body body)
  `(progn
     (setf (gethash ',name *walker-handlers*)
           (named-lambda ,(format-symbol *package* "WALKER-HANDLER/~A" name)
               (,form ,parent ,lexenv)
             (declare (ignorable ,parent ,lexenv))
             ,@body))
     ',name))

(defmacro defwalker-handler-alias (from-name to-name)
  `(progn
     (setf (gethash ',to-name *walker-handlers*) (gethash ',from-name *walker-handlers*))
     ',to-name))

(defmacro defunwalker-handler (class (&rest slots) &body body)
  `(progn
     (defmethod unwalk-form ((-form- ,class))
       (with-slots ,slots -form-
         ,@body))
     ',class))

(defclass form ()
  ((parent :accessor parent-of :initarg :parent)
   (source :accessor source-of :initarg :source)))

(defmethod make-load-form ((object form) &optional env)
  (make-load-form-saving-slots object :environment env))

(defmethod print-object ((form form) stream)
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
