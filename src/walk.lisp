;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See COPYING for details.

(in-package :cl-walker)

(defclass declaration-form (form)
  ())

(defclass optimize-declaration-form (declaration-form)
  ((optimize-spec :accessor optimize-spec :initarg :optimize-spec)))

(defclass variable-declaration-form (declaration-form)
  ((name :accessor name-of :initarg :name)))

(defclass function-declaration-form (declaration-form)
  ((name :accessor name-of :initarg :name)))

(defclass dynamic-extent-declaration-form (variable-declaration-form)
  ())

(defclass ignorable-declaration-form-mixin (declaration-form)
  ())

(defclass variable-ignorable-declaration-form (variable-declaration-form ignorable-declaration-form-mixin)
  ())

(defclass function-ignorable-declaration-form (function-declaration-form ignorable-declaration-form-mixin)
  ())

(defclass special-declaration-form (variable-declaration-form)
  ())

(defclass type-declaration-form (variable-declaration-form)
  ((type-form :accessor type-form :initarg :type-form)))

(defclass ftype-declaration-form (function-declaration-form)
  ((type-form :accessor type-form :initarg :type-form)))

(defclass notinline-declaration-form (function-declaration-form)
  ())

(defun parse-declaration (declaration environment parent)
  (let ((declares nil))
    (flet ((funname (form)
             (if (and (consp form) (eql (car form) 'function))
                 (cadr form)
                 nil)))
      (macrolet ((mkdecl (varname formclass &rest rest)
                   `(make-instance ,formclass :parent parent :source (list type ,varname) ,@rest))
                 (extend-env ((var list) newdeclare &rest datum)
                   `(dolist (,var ,list)
                      (when ,newdeclare (push ,newdeclare declares))
                      (extend-walk-env environment :declare ,@datum))))
        (destructuring-bind (type &rest arguments)
            declaration
          (case type
            (dynamic-extent
             (extend-env (var arguments)
                         (mkdecl var 'dynamic-extent-declaration-form :name var)
                         var `(dynamic-extent)))
            (ftype
             (extend-env (function-name (cdr arguments))
                         (make-instance 'ftype-declaration-form
                                        :parent parent
                                        :source `(ftype ,(first arguments) function-name)
                                        :name function-name
                                        :type-form (first arguments))
                         function-name `(ftype ,(first arguments))))
            ((ignore ignorable)
             (extend-env (var arguments)
                         (aif (funname var)
                              (mkdecl var 'function-ignorable-declaration-form :name it)
                              (mkdecl var 'variable-ignorable-declaration-form :name var))
                         var `(ignorable)))
            (inline
              (extend-env (function arguments)
                          (mkdecl function 'function-ignorable-declaration-form :name function)
                          function `(ignorable)))
            (notinline
             (extend-env (function arguments)
                         (mkdecl function 'notinline-declaration-form :name function)
                         function `(notinline)))
            (optimize
             (extend-env (optimize-spec arguments)
                         (mkdecl optimize-spec 'optimize-declaration-form :optimize-spec optimize-spec)
                         'optimize optimize-spec))
            (special
             (extend-env (var arguments)
                         (mkdecl var 'special-declaration-form :name var)
                         var `(special)))
            (type
             (extend-env (var (rest arguments))
                         (make-instance 'type-declaration-form
                                        :parent parent
                                        :source `(type ,(first arguments) ,var)
                                        :name var
                                        :type-form (first arguments))
                         var `(type ,(first arguments))))
            (t
             ;; TODO weak try: assumes everything else is a type declaration
             (extend-env (var arguments)
                         (make-instance 'type-declaration-form
                                        :parent parent
                                        :source `(,type ,var)
                                        :name var
                                        :type-form type)
                         var `(type ,type)))))))
    ;; TODO this generates wrong ast for (walk-form '(lambda () (declare (ignorable))))
    (when (null declares)
      (setq declares (list (make-instance 'declaration-form :parent parent :source declaration))))
    (values environment declares)))

(defun walk-implict-progn (parent forms env &key docstring declare)
  (handler-bind ((undefined-reference (lambda (condition)
                                        (unless (enclosing-code condition)
                                          (setf (enclosing-code condition) `(progn ,@forms))))))
    (multiple-value-bind (body env docstring declarations)
        (split-body forms env :parent parent :docstring docstring :declare declare)
      (values (mapcar (lambda (form)
                        (walk-form form parent env))
                      body)
              docstring
              declarations))))

;;;; Atoms

(defclass constant-form (form)
  ((value :accessor value :initarg :value)))

(defclass variable-reference (form)
  ((name :accessor name-of :initarg :name)))

(defmethod print-object ((v variable-reference) stream)
  (print-unreadable-object (v stream :type t :identity t)
    (format stream "~S" (name-of v))))

(defclass local-variable-reference (variable-reference)
  ())

(defclass local-lexical-variable-reference (local-variable-reference)
  ()
  (:documentation "A reference to a local variable defined in the
  lexical environment outside of the form passed to walk-form."))

(defclass free-variable-reference (variable-reference)
  ())

(defwalker-handler +atom-marker+ (form parent env)
  (cond
    ((not (or (symbolp form) (consp form)))
     (make-instance 'constant-form :value form
                    :parent parent :source form))
    ((lookup-walk-env env :let form)
     (make-instance 'local-variable-reference :name form
                    :parent parent :source form))
    ((lookup-walk-env env :lexical-let form)
     (make-instance 'local-lexical-variable-reference :name form
                    :parent parent :source form))
    ((lookup-walk-env env :symbol-macrolet form)
     (walk-form (lookup-walk-env env :symbol-macrolet form) parent env))
    ((nth-value 1 (macroexpand-1 form))
     ;; a globaly defined symbol-macro
     (walk-form (macroexpand-1 form) parent env))
    (t
     (when (and *warn-undefined*
                (not (boundp form)))
       (warn 'undefined-variable-reference :name form))
     (make-instance 'free-variable-reference :name form
                    :parent parent :source form))))

;;;; Function Applictation

(defclass application-form (form)
  ((operator :accessor operator :initarg :operator)
   (arguments :accessor arguments :initarg :arguments)))

(defclass local-application-form (application-form)
  ((code :accessor code :initarg :code)))

(defclass lexical-application-form (application-form)
  ())

(defclass free-application-form (application-form)
  ())

(defclass lambda-application-form (application-form)
  ())

(defwalker-handler application (form parent env)
  (block nil
    (destructuring-bind (op &rest args)
        form
      (when (and (consp op)
                 (eq 'cl:lambda (car op)))
        (return
          (with-form-object (application lambda-application-form :parent parent :source form)
            (setf (operator application) (walk-form op application env)
                  (arguments application) (mapcar (lambda (form)
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
        (setf (operator app) op
              (parent app) parent
              (source app) form
              (arguments app) (mapcar (lambda (form)
                                        (walk-form form app env))
                                      args))
        app))))

;;;; Functions

(defclass function-form (form)
  ())

(defclass lambda-function-form (function-form implicit-progn-with-declare-mixin)
  ((arguments :accessor arguments :initarg :arguments)))

(defclass function-object-form (form)
  ((name :accessor name-of :initarg :name)))

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
    (multiple-value-setf ((arguments func) env)
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

(defgeneric required-function-argument-form-p (object)
  (:method ((object t)) nil)
  (:method ((object required-function-argument-form)) t))

(defun walk-required-argument (form parent env)
  (declare (ignore env))
  (make-instance 'required-function-argument-form
                 :name form
                 :parent parent :source form))

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

(defclass allow-other-keys-function-argument-form (function-argument-form)
  ())

(defclass rest-function-argument-form (function-argument-form)
  ())

(defun walk-rest-argument (form parent env)
  (declare (ignore env))
  (make-instance 'rest-function-argument-form :name form
                 :parent parent :source form))

;;;; BLOCK/RETURN-FROM

(defclass block-form (form implicit-progn-mixin)
  ((name :accessor name-of :initarg :name)))

(defclass return-from-form (form)
  ((target-block :accessor target-block :initarg :target-block)
   (result :accessor result :initarg :result)))

(defwalker-handler block (form parent env)
  (destructuring-bind (block-name &rest body)
      (cdr form)
    (with-form-object (block block-form
                       :parent parent :source form
                       :name block-name)
      (setf (body block) (walk-implict-progn block
                                             body
                                             (register-walk-env env :block block-name block))))))

(define-condition return-from-unknown-block (error)
  ((block-name :accessor block-name :initarg :block-name))
  (:report (lambda (condition stream)
             (format stream "Unable to return from block named ~S." (block-name condition)))))

(defwalker-handler return-from (form parent env)
  (destructuring-bind (block-name &optional (value '(values)))
      (cdr form)
    (if (lookup-walk-env env :block block-name)
        (with-form-object (return-from return-from-form :parent parent :source form
                           :target-block (lookup-walk-env env :block block-name))
          (setf (result return-from) (walk-form value return-from env)))
        (restart-case
            (error 'return-from-unknown-block :block-name block-name)
          (add-block ()
            :report "Add this block and continue."
            (walk-form form parent (register-walk-env env :block block-name :unknown-block)))))))

;;;; CATCH/THROW

(defclass catch-form (form implicit-progn-mixin)
  ((tag :accessor tag :initarg :tag)))

(defclass throw-form (form)
  ((tag :accessor tag :initarg :tag)
   (value :accessor value :initarg :value)))

(defwalker-handler catch (form parent env)
  (destructuring-bind (tag &body body)
      (cdr form)
    (with-form-object (catch catch-form :parent parent :source form)
      (setf (tag catch) (walk-form tag catch env)
            (body catch) (walk-implict-progn catch body env)))))

(defwalker-handler throw (form parent env)
  (destructuring-bind (tag &optional (result '(values)))
      (cdr form)
    (with-form-object (throw throw-form :parent parent :source form)
      (setf (tag throw) (walk-form tag throw env)
            (value throw) (walk-form result throw env)))))

;;;; EVAL-WHEN

(defclass eval-when-form (form implicit-progn-mixin)
  ((eval-when-times :accessor eval-when-times :initarg :eval-when-times)))

(defwalker-handler eval-when (form parent env)
  (destructuring-bind (times &body body)
      (cdr form)
    (with-form-object (eval-when eval-when-form :parent parent :source form)
      (setf (eval-when-times eval-when) times
            (body eval-when) (walk-implict-progn eval-when body env)))))

;;;; IF

(defclass if-form (form)
  ((consequent :accessor consequent :initarg :consequent)
   (then :accessor then :initarg :then)
   (else :accessor else :initarg :else)))

(defwalker-handler if (form parent env)
  (with-form-object (if if-form :parent parent :source form)
    (setf (consequent if) (walk-form (second form) if env)
          (then if) (walk-form (third form) if env)
          (else if) (walk-form (fourth form) if env))))

;;;; FLET/LABELS

(defclass function-binding-form (form binding-form-mixin implicit-progn-with-declare-mixin)
  ())

(defclass flet-form (function-binding-form)
  ())

(defclass labels-form (function-binding-form)
  ())

(defwalker-handler flet (form parent env)
  (destructuring-bind (binds &body body)
      (cdr form)
    (with-form-object (flet flet-form :parent parent :source form)
      ;;;; build up the objects for the bindings in the original env
      (loop
         for (name args . body) in binds
         collect (cons name (walk-form `(lambda ,args ,@body) flet env)) into bindings
         finally (setf (binds flet) bindings))
      ;;;; walk the body in the new env
      (multiple-value-setf ((body flet) nil (declares flet))
                           (walk-implict-progn flet
                                               body
                                               (loop
                                                  with env = env
                                                  for (name . lambda) in (binds flet)
                                                  do (extend-walk-env env :flet name lambda)
                                                  finally (return env))
                                               :declare t)))))

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
         for (name arguments . body) in binds
         for lambda = (make-instance 'lambda-function-form
                                     :parent labels
                                     :source (list* name arguments body))
         do (push (cons name lambda) (binds labels))
         do (extend-walk-env env :flet name lambda))
      (setf (binds labels) (nreverse (binds labels)))
      (loop
         for form in binds
         for (arguments . body) = (cdr form)
         for binding in (binds labels)
         for lambda = (cdr binding)
         for tmp-lambda = (walk-lambda `(lambda ,arguments ,@body) labels env)
         do (setf (body lambda) (body tmp-lambda)
                  (arguments lambda) (arguments tmp-lambda)
                  (declares lambda) (declares tmp-lambda)))
      (multiple-value-setf ((body labels) nil (declares labels)) (walk-implict-progn labels body env :declare t)))))

;;;; LET/LET*

(defclass variable-binding-form (form binding-form-mixin implicit-progn-with-declare-mixin)
  ())

(defclass let-form (variable-binding-form)
  ())

(defwalker-handler let (form parent env)
  (with-form-object (let let-form :parent parent :source form)
    (setf (binds let) (mapcar (lambda (binding)
                                   (destructuring-bind (var &optional initial-value)
                                       (ensure-list binding)
                                     (cons var (walk-form initial-value let env))))
                                 (second form)))
    (multiple-value-bind (b e d declarations)
        (split-body (cddr form) env :parent let :declare t)
      (declare (ignore b e d))
      (loop for (var . value) :in (binds let) do
            (unless (find-if (lambda (declaration)
                               (and (typep declaration 'special-declaration-form)
                                    (eq var (name-of declaration)))) declarations)
              (extend-walk-env env :let var :dummy)))
      (multiple-value-setf ((body let) nil (declares let))
                           (walk-implict-progn let (cddr form) env :declare t)))))

(defclass let*-form (variable-binding-form)
  ())

(defwalker-handler let* (form parent env)
  (with-form-object (let* let*-form :parent parent :source form :binds '())
    (dolist* ((var &optional initial-value) (mapcar #'ensure-list (second form)))
      (push (cons var (walk-form initial-value let* env)) (binds let*))
      (extend-walk-env env :let var :dummy))
    (setf (binds let*) (nreverse (binds let*)))
    (multiple-value-setf ((body let*) nil (declares let*)) (walk-implict-progn let* (cddr form) env :declare t))))

;;;; LOAD-TIME-VALUE

(defclass load-time-value-form (form)
  ((value :accessor value)
   (read-only-p :accessor read-only-p)))

(defwalker-handler load-time-value (form parent env)
  (with-form-object (load-time-value load-time-value-form
                                     :parent parent :source form)
    (setf (value load-time-value) (walk-form (second form) load-time-value env)
          (read-only-p load-time-value) (third form))))

;;;; LOCALLY

(defclass locally-form (form implicit-progn-with-declare-mixin)
  ())

(defwalker-handler locally (form parent env)
  (with-form-object (locally locally-form :parent parent :source form)
    (multiple-value-setf ((body locally) nil (declares locally)) (walk-implict-progn locally (cdr form) env :declare t))))

;;;; MACROLET

(defclass macrolet-form (form binding-form-mixin implicit-progn-with-declare-mixin)
  ())

(defwalker-handler macrolet (form parent env)
  (with-form-object (macrolet macrolet-form :parent parent :source form
                              :binds '())
    (dolist* ((name args &body body) (second form))
      (let ((handler (parse-macro-definition name args body (cdr env))))
        (extend-walk-env env :macrolet name handler)
        (push (cons name handler) (binds macrolet))))
    (setf (binds macrolet) (nreverse (binds macrolet)))
    (multiple-value-setf ((body macrolet) nil (declares macrolet))
      (walk-implict-progn macrolet (cddr form) env :declare t))))

;;;; MULTIPLE-VALUE-CALL

(defclass multiple-value-call-form (form)
  ((func :accessor func :initarg :func)
   (arguments :accessor arguments :initarg :arguments)))

(defwalker-handler multiple-value-call (form parent env)
  (with-form-object (m-v-c multiple-value-call-form :parent parent :source form)
    (setf (func m-v-c) (walk-form (second form) m-v-c env)
          (arguments m-v-c) (mapcar (lambda (f) (walk-form f m-v-c env))
                                    (cddr form)))))

;;;; MULTIPLE-VALUE-PROG1

(defclass multiple-value-prog1-form (form)
  ((first-form :accessor first-form :initarg :first-form)
   (other-forms :accessor other-forms :initarg :other-forms)))

(defwalker-handler multiple-value-prog1 (form parent env)
  (with-form-object (m-v-p1 multiple-value-prog1-form :parent parent :source form)
    (setf (first-form m-v-p1) (walk-form (second form) m-v-p1 env)
          (other-forms m-v-p1) (mapcar (lambda (f) (walk-form f m-v-p1 env))
                                       (cddr form)))))

;;;; PROGN

(defclass progn-form (form implicit-progn-mixin)
  ())

(defwalker-handler progn (form parent env)
  (with-form-object (progn progn-form :parent parent :source form)
    (setf (body progn) (walk-implict-progn progn (cdr form) env))))

;;;; PROGV

(defclass progv-form (form implicit-progn-mixin)
  ((vars-form :accessor vars-form :initarg :vars-form)
   (values-form :accessor values-form :initarg :values-form)))

(defwalker-handler progv (form parent env)
  (with-form-object (progv progv-form :parent parent :source form)
    (setf (vars-form progv) (walk-form (cadr form) progv env))
    (setf (values-form progv) (walk-form (caddr form) progv env))
    (setf (body progv) (walk-implict-progn progv (cdddr form) env))
    progv))

;;;; QUOTE

(defwalker-handler quote (form parent env)
  (make-instance 'constant-form :parent parent :source form :value (second form)))

;;;; SETQ

(defclass setq-form (form)
  ((variable-name
    :accessor variable-name-of
    :initarg :variable-name)
   (value
    :accessor value
    :initarg :value)))

(defwalker-handler setq (form parent env)
  ;; the SETQ handler needs to be able to deal with symbol-macrolets
  ;; which haven't yet been expanded and may expand into something
  ;; requiring setf and not setq.
  (let ((effective-code '()))
    (loop
       for (name value) on (cdr form) by #'cddr
       if (lookup-walk-env env :symbol-macrolet name)
         do (push `(setf ,(lookup-walk-env env :symbol-macrolet name) ,value) effective-code)
       else
         do (push `(setq ,name ,value) effective-code))
    (if (= 1 (length effective-code))
        ;; only one form, the "simple case"
        (destructuring-bind (type var value)
            (first effective-code)
          (ecase type
            (setq (with-form-object (setq setq-form :parent parent :source form
                                          :variable-name var)
                    (setf (value setq) (walk-form value setq env))))
            (setf (walk-form (first effective-code) parent env))))
        ;; multiple forms
        (with-form-object (progn progn-form :parent parent :source form)
          (setf (body progn) (walk-implict-progn progn effective-code env))))))

;;;; SYMBOL-MACROLET

(defclass symbol-macrolet-form (form binding-form-mixin implicit-progn-with-declare-mixin)
  ())

(defwalker-handler symbol-macrolet (form parent env)
  (with-form-object (symbol-macrolet symbol-macrolet-form :parent parent :source form
                                     :binds '())
    (dolist* ((symbol expansion) (second form))
      (extend-walk-env env :symbol-macrolet symbol expansion)
      (push (cons symbol expansion) (binds symbol-macrolet)))
    (setf (binds symbol-macrolet) (nreverse (binds symbol-macrolet)))
    (multiple-value-setf ((body symbol-macrolet) nil (declares symbol-macrolet))
      (walk-implict-progn symbol-macrolet (cddr form) env :declare t))))

;;;; TAGBODY/GO

(defclass tagbody-form (form implicit-progn-mixin)
  ())

(defclass go-tag-form (form)
  ((name :accessor name-of :initarg :name)))

(defgeneric go-tag-form-p (object)
  (:method ((object go-tag-form)) t)
  (:method ((object t))           nil))

(defwalker-handler tagbody (form parent env)
  (with-form-object (tagbody tagbody-form :parent parent :source form :body (cdr form))
    (extend-walk-env env :tagbody 'enclosing-tagbody tagbody)
    (flet ((go-tag-p (form)
             (or (symbolp form) (integerp form))))
      ;; the loop below destructuivly modifies the body of tagbody,
      ;; since it's the same object as the source we need to copy it.
      (setf (body tagbody) (copy-list (body tagbody)))
      (loop
         for part on (body tagbody)
         if (go-tag-p (car part))
           do (extend-walk-env env :tag (car part) (cdr part)))
      (loop
         for part on (body tagbody)
         if (go-tag-p (car part))
           do (setf (car part) (make-instance 'go-tag-form :parent tagbody
                                              :source (car part)
                                              :name (car part)))
         else
           do (setf (car part) (walk-form (car part) tagbody env))))))

(defclass go-form (form)
  ((jump-target :accessor jump-target-of :initarg :jump-target)
   (name :accessor name-of :initarg :name)
   (enclosing-tagbody :accessor enclosing-tagbody-of :initarg :enclosing-tagbody)))

(defwalker-handler go (form parent env)
  (make-instance 'go-form
                 :parent parent
                 :source form
                 :name (second form)
                 :jump-target (lookup-walk-env env :tag (second form))
                 :enclosing-tagbody (lookup-walk-env env :tagbody 'enclosing-tagbody)))

;;;; THE

(defclass the-form (form)
  ((type-form :accessor type-form :initarg :type-form)
   (value :accessor value :initarg :value)))

(defwalker-handler the (form parent env)
  (with-form-object (the the-form :parent parent :source form
                                  :type-form (second form))
    (setf (value the) (walk-form (third form) the env))))

;;;; UNWIND-PROTECT

(defclass unwind-protect-form (form)
  ((protected-form :accessor protected-form :initarg :protected-form)
   (cleanup-form :accessor cleanup-form :initarg :cleanup-form)))

(defwalker-handler unwind-protect (form parent env)
  (with-form-object (unwind-protect unwind-protect-form :parent parent
                                    :source form)
    (setf (protected-form unwind-protect) (walk-form (second form) unwind-protect env)
          (cleanup-form unwind-protect) (walk-implict-progn unwind-protect (cddr form) env))))

;;;; LOAD-TIME-VALUE

(defclass load-time-value-form (form)
  ((body :accessor body :initarg :body)
   (read-only :initform nil :accessor read-only-p :initarg :read-only)
   (value :accessor value)))

(defmethod initialize-instance :after ((self load-time-value-form) &key)
  (setf (value self) (eval (body self))))

(defwalker-handler load-time-value (form parent env)
  (assert (<= (length form) 3))
  (with-form-object (load-time-value load-time-value-form :parent parent
                                     :body form
                                     :read-only (third form))
    (setf (body load-time-value) (walk-form (second form)))))
