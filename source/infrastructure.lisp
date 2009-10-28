;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.walker)

(def (condition* e) walker-error (error)
  ())

(def (condition* e) simple-walker-error (simple-error walker-error)
  ())

(def function simple-walker-error (message &rest args)
  (error 'simple-walker-error :format-control message :format-arguments args))

(def function macroexpand-all (form &optional (env (make-empty-lexical-environment)))
  (unwalk-form (walk-form form :environment (make-walk-environment env))))

(def special-variable *current-form* nil)
(def special-variable *inside-macroexpansion* nil)

(def macro with-current-form (form &body body)
  (once-only (form)
    `(bind ((*current-form* (if (and *inside-macroexpansion*
                                     *current-form*)
                                *current-form*
                                ,form)))
       ,@body)))

(def (layered-function e) walk-form (form &key parent environment)
  (:documentation "Entry point to initiate code walking of FORM using ENVIRONMENT. Returns a CLOS based AST that represents FORM.")
  (:method (form &key parent environment)
    (unless environment
      (setf environment (make-walk-environment)))
    (bind ((*current-form* (or *current-form*
                               form)))
      (with-current-form form
        (funcall (find-walker-handler form) form parent environment)))))

(def layered-function coerce-to-form (form)
  (:method (form)
    form))

(def (layered-function e) unwalk-form (form)
  (:documentation "Unwalk FORM and return a list representation."))

(def (function e) unwalk-forms (forms)
  (mapcar #'unwalk-form forms))

(def function eval (form)
  (bind (#+sbcl(sb-ext:*evaluator-mode* :interpret))
    (common-lisp:eval form)))

(def function special-variable-name? (name)
  (and (symbolp name)
       (not (keywordp name))
       (not (member name '(t nil) :test #'eq))
       (or (boundp name)
           #+sbcl(eq (sb-int:info :variable :kind name) :special)
           #+lispworks(eq (common-lisp::variable-information name) :special)
           ;; This is the only portable way to check if a symbol is
           ;; declared special, without being boundp, i.e. (defvar 'foo).
           ;; Maybe we should make it optional with a compile-time flag?
           #+nil
           (eval `((lambda ()
                     (flet ((func ()
                              (symbol-value ',var)))
                       (let ((,var t))
                         (declare (ignorable ,var))
                         (ignore-errors (func))))))))))

(defun collect-standard-walked-form-subclasses ()
  "Returns a list of all the subclasses of hu.dwim.walker:walked-form whose name is in the hu.dwim.walker package. This is useful if you want to generate a complete AST-NODE-TYPE-MAPPING hashtable with a mixin in the class of each walked node."
  (remove-duplicates
   (remove-if (lambda (class)
                (not (eq (symbol-package (class-name class)) #.(find-package :hu.dwim.walker))))
              (labels ((collect-subclasses (class)
                         (let ((direct-subclasses (closer-mop:class-direct-subclasses class)))
                           (nconc
                            (copy-list direct-subclasses)
                            (loop
                               :for subclass :in direct-subclasses
                               :nconc (collect-subclasses subclass))))))
                (collect-subclasses (find-class 'walked-form))))))

(def layered-function function-name? (name)
  (:method (name)
    (or #+sbcl(eq (sb-int:info :function :kind name) :function)
        (fboundp name))))

(def layered-function macro-name? (name &optional env)
  (:method (name &optional env)
    (macro-function name env)))

(def layered-function symbol-macro-name? (name &optional env)
  (:method (name &optional env)
    (nth-value 1 (macroexpand-1 name env))))

(def layered-function constant-name? (form &optional env)
  (:method (form &optional env)
    (declare (ignore env))
    (or (eq form t)
        (eq form nil)
        (keywordp form)
        (not (or (symbolp form)
                 (consp form))))))

(def layered-function lambda-form? (form &optional env)
  (:method (form &optional env)
    (declare (ignore env))
    (and (consp form)
         (eq 'lambda (car form)))))

(def layered-function walker-macroexpand-1 (form &optional env)
  (:method (form &optional env)
    (macroexpand-1 form env)))

(def (layer e) ignore-undefined-references ()
  ())

(def layered-function handle-undefined-reference (type name)
  (:method (type name)
    (ecase type
      (:function (warn 'undefined-function-reference :name name))
      (:variable (warn 'undefined-variable-reference :name name))))
  (:method :in ignore-undefined-references (type name)
    ;; well, we ignore them in this layer...
    ))

;;;
;;; Walk environment
;;;

;; there are three players here:
;; 1) the walkenv, which contains the already walked *-form instances
;; 2) the lexenv, which is the underlying lisp's internal lexenv
;; 3) the combined environment, which is (cons walkenv lexenv)

;; TODO fix naming! currently walkenv is a (cons walkenv lexenv)?!

(defun make-walk-environment (&optional lexenv)
  (unless lexenv
    (setf lexenv (make-empty-lexical-environment)))
  (let ((walkenv '()))
    (macrolet ((extend! (environment type name datum &rest other-datum)
                 `(setf ,environment (%environment/augment ,environment ,type ,name ,datum ,@other-datum))))
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

(def function %augment (env type name &optional datum)
  (bind ((walkenv (%environment/augment (car env) type name datum))
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

;; TODO get rid of this, or rename, or something...
(defmacro augment-walkenv! (env type name datum &rest other-datum)
  `(setf ,env (%augment ,env ,type ,name ,datum ,@other-datum)))

(def function %environment/augment (environment type name datum &rest other-datum)
  (cons (if other-datum
            (list* type name datum other-datum)
            (list* type name datum))
        environment))

(def function %environment/find (environment type name &key (otherwise nil))
  (loop
     :for (.type .name . data) :in environment
     :when (and (eq .name name)
                (or (null type)
                    (eq .type type)
                    (and (listp type)
                         (member .type type :test #'eq))))
       :return (if (listp type)
                   (values .type .name data)
                   (values data t))
     :finally
       (return
         (if (eq otherwise :error)
             (error "No value for ~S of type ~S in environment ~S was found."
                    name type environment)
             (handle-otherwise otherwise)))))

#+(or) ;; it's not used for now
(defun (setf %environment/find) (value environment type name &key (error-p nil))
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

(def special-variable *walker-handlers* (make-hash-table :test 'eq))

(def (condition* e) undefined-reference (style-warning)
  ((enclosing-code *current-form*)
   (name)))

(def (condition* e) undefined-variable-reference (undefined-reference)
  ()
  (:report
   (lambda (c stream)
     (if (enclosing-code-of c)
         (format stream "Reference to unknown variable ~S in ~S." (name-of c) (enclosing-code-of c))
         (format stream "Reference to unknown variable ~S." (name-of c))))))

(def (condition* e) undefined-function-reference (undefined-reference)
  ()
  (:report
   (lambda (c stream)
     (if (enclosing-code-of c)
         (format stream "Reference to unknown function ~S in ~S." (name-of c) (enclosing-code-of c))
         (format stream "Reference to unknown function ~S." (name-of c))))))

(def constant +atom-marker+ '+atom-marker+)

(def (layered-function e) find-walker-handler (form)
  (:documentation "Simple function which tells us what handler should deal with FORM. Signals an error if we don't have a handler for FORM.")
  (:method ((form cons))
    (or (gethash (car form) *walker-handlers*)
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

(def function walker-handler-definition (name &optional (table *walker-handlers*))
  (gethash name table))

(def function (setf walker-handler-definition) (handler name &optional (table *walker-handlers*))
  (when (gethash name table)
    (simple-style-warning "Redefining walker handler for ~S" name))
  (setf (gethash name table) handler))

(def function %walker-handler-body (name body &optional declarations)
  (bind ((function-name (format-symbol *package* "WALKER-HANDLER/~A" name)))
    `(progn
       (defun ,function-name (-form- -parent- -environment-)
         (declare (ignorable -parent- -environment-)
                  ,@declarations)
         (block nil
           (bind ((-form- (coerce-to-form -form-)))
             (macrolet ((-lookup- (type name &key (otherwise nil))
                          `(%environment/find (car -environment-) ,type ,name :otherwise ,otherwise))
                        (-augment- (type name &optional datum)
                          `(setf -environment- (%augment -environment- ,type ,name ,datum))))
               (flet ((recurse (node &optional (parent -parent-) (environment -environment-))
                        (walk-form node :parent parent :environment environment)))
                 (declare (ignorable #'recurse))
                 (with-current-form -form-
                   ,@body))))))
       (setf (walker-handler-definition ',name) ',function-name)
       ',name)))

(def (macro e) defwalker-handler (name &body body)
  (%walker-handler-body name body))

(def (definer e :available-flags "od") walker (name &body body)
  (with-standard-definer-options name
    (%walker-handler-body name body (function-like-definer-declarations -options-))))

(def function %unwalker-handler-body (class slots body &optional declarations)
  `(progn
     (def layered-method unwalk-form ((-form- ,class))
       ,@(when declarations
         `((declare ,@declarations)))
       (block nil
         (flet ((recurse (node)
                  (unwalk-form node))
                (recurse-on-body (nodes)
                  (mapcar #'unwalk-form nodes)))
           (declare (ignorable #'recurse #'recurse-on-body))
           (with-slots ,slots -form-
             ,@body))))
     ',class))

(def (macro e) defunwalker-handler (class (&rest slots) &body body)
  (%unwalker-handler-body class slots body))

(def (definer e :available-flags "od") unwalker (class (&rest slots) &body body)
  (%unwalker-handler-body class slots body (function-like-definer-declarations -options-)))

(def macro defwalker-handler-alias (from-name to-name)
  `(progn
     (setf (walker-handler-definition ',to-name) (walker-handler-definition ',from-name))
     ',to-name))

(def (definer e) walker-alias (from-name to-name)
  `(defwalker-handler-alias ,from-name ,to-name))

(def (class* e) walked-form ()
  ((parent)
   (source *current-form*)))

(def (class* e) named-walked-form (walked-form)
  ((name)))

(def method make-load-form ((object walked-form) &optional env)
  (make-load-form-saving-slots object :environment env))

(def print-object walked-form
  (if (and (slot-boundp -self- 'source)
           (source-of -self-))
      (let ((*print-readably* nil)
            (*print-level* 0)
            (*print-length* 4))
        (format t "~S" (source-of -self-)))
      (call-next-method)))

(def layered-function ast-node-type-for (type)
  (:method (type)
    type))

(def macro make-form-object (type parent &rest initargs)
  (with-unique-names (custom-type)
    (appendf initargs `(:parent ,parent))
    `(bind ((,custom-type (ast-node-type-for ,type)))
       ;; do it this way, so that we'll have an optimized (make-instance 'literal ...) path
       (if ,custom-type
           (make-instance ,custom-type ,@initargs)
           (make-instance ,type ,@initargs)))))

(def macro with-form-object ((variable type parent &rest initargs)
                            &body body)
  `(bind ((,variable (make-form-object ,type ,parent ,@initargs)))
     ,@body
     ,variable))

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
                   (when (symbolp variable)
                     ;; TODO protect against brokenness, see TEST/MACRO/1
                     ;; it does not handle destructuring bind, which is available for macro lambda args
                     (augment-lexenv! :variable variable lexenv))))
               (mapcar (lambda (form)
                         (macroexpand-all form lexenv))
                       body))))))))
