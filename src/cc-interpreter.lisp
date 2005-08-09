;;;; -*- lisp -*-

(in-package :it.bese.arnesi)

;;;; * A Common Lisp interpreter with support for continuations.

;;;; Notes:

;;;; This interpreter is dependnt on the object tree built up by the
;;;; code walker in walk.lisp.

;;;; One of the, final, goals of this interpeter was to allow
;;;; continuations to be serializable. Due to this constraint we
;;;; represent continuations as regular lists which, when the cdr
;;;; (which must be clos objects or literals) is applied to the car
;;;; (which must be a symbol) the actual contiunation (a regular
;;;; common lisp function) is returned. 

(defvar *call/cc-returns* nil)

(defmacro with-call/cc (&environment e &body body)
  "Execute BODY with delimited partial continuations.

  Within the code of BODY almest all common lisp forms maintain
  their normal semantics. The following special forms are
  allowed:

  (call/cc LAMBDA) - LAMBDA, a one orgument function, will be
  passed a continutaion. This object may then be passed to the
  function KALL which will cause execution to resume around the
  call/cc form. "
  (let ((walk-env (make-walk-env e))
        (evaluate-env nil))
    (dolist* ((type name &rest data) walk-env)
      (declare (ignore data))
      (when (eql :lexical-let type)
        (push (list 'list
                    :lexical-let
                    `(quote ,name)
                    ;; NB: this makes the environment, and therefore
                    ;; contiunations, unserializable. we would need to
                    ;; change this to a regular :let and not allow the
                    ;; setting of lexical variables. 
                    `(lambda () ,name)
                    (with-unique-names (v)
                      `(lambda (,v) (setf ,name ,v))))
              evaluate-env)))
    (setf evaluate-env `(list ,@(nreverse evaluate-env)))
    `(drive-cps (evaluate/cps ,(walk-form (if (rest body)
                                              `(progn ,@body)
                                              (first body))
                                          nil walk-env)
                              ,evaluate-env
                              *toplevel-k*))))

(defun kall (k &optional (primary-value nil primary-value-p)
               &rest other-values)
  "Continue the continuation K.

This function can be used within the lexcial scope of
with-call/cc and outside, though it has slightly different
semantics."
  (drive-cps (lambda ()
               (let ((k (apply (car k) (cdr k))))
                 (cond
                   (other-values (apply k primary-value other-values))
                   (primary-value-p (funcall k primary-value))
                   (t (funcall k nil)))))))

;;;; Implementation

(defun drive-cps (cps)
  (catch 'done
    (loop for func = cps then (funcall func))))

(defmacro let/cc (k &body body)
  `(call/cc (lambda (,k) ,@body)))

(defmacro retk ()
  `(let/cc k k))

(defmacro klambda ((&optional (value (gensym) valuep) (other-values (gensym) other-values-p))
                   &body body)
  (cond
    (other-values-p `(lambda (,value &rest ,other-values)
                       (lambda () ,@body)))
    (valuep `(lambda (,value &rest ,other-values)
               (declare (ignore ,other-values))
               (lambda () ,@body)))
    (t `(lambda (,value &rest ,other-values)
          (declare (ignore ,value ,other-values))
          (lambda () ,@body)))))

(defun kontinue (k &optional (primary-value nil primary-value-p)
                 &rest other-values)
  (let ((k (apply (car k) (cdr k))))
    (cond
      (other-values (apply k primary-value other-values))
      (primary-value-p (funcall k primary-value))
      (t (funcall k)))))

(defmacro defk (name args k-args &body body)
  `(defun ,name ,args
     (declare (ignorable ,@args))
     (klambda ,k-args ,@body)))

(defgeneric evaluate/cps (form env k))

(defvar *k*)

(defun toplevel-k ()
  (klambda (value other-values)
    (throw 'done (values-list (cons value other-values)))))

(defparameter *toplevel-k* '(toplevel-k))

;;;; Variable References

(defmethod evaluate/cps ((var local-variable-reference) env k)
  (kontinue k (lookup env :let (name var) :error-p t)))

(defmethod evaluate/cps ((var local-lexical-variable-reference) env k)
  (kontinue k (funcall (first (lookup env :lexical-let (name var) :error-p t)))))

(defmethod evaluate/cps ((var free-variable-reference) env k)
  (declare (ignore env))
  (kontinue k (symbol-value (name var))))

;;;; FUNCTION and LAMBDA form

(defmethod evaluate/cps ((func free-function-object-form) env k)
  (declare (ignore env))
  (if (fboundp (name func))
      (kontinue k (fdefinition (name func)))
      (error "Unbound function ~S." (name func))))

(defmethod evaluate/cps ((func local-function-object-form) env k)
  (kontinue k (lookup env :flet (name func) :error-p t)))

(defclass cps-closure ()
  ((code :accessor code :initarg :code)
   (env :accessor env :initarg :env)))

(defmethod evaluate/cps ((lambda lambda-function-form) env k)
  (kontinue k (make-instance 'cps-closure :code lambda :env env)))

(defk k-for-call/cc (k)
    (value)
  (if *call/cc-returns*
      (kontinue k value)
      (throw 'done value)))

(defmethod evaluate/cps ((func free-application-form) env k)
  (cond 
    ((eql 'call/cc (operator func))
     (evaluate/cps (make-instance 'free-application-form
                                  :operator 'funcall
                                  :arguments (list (first (arguments func)) (make-instance 'constant-form :value k :source k))
                                  :source (source func))
                   env `(k-for-call/cc ,k)))
    
    ((eql 'kall (operator func))
     (evaluate-arguments-then-apply
      (lambda (arguments)
        (apply #'kontinue (first arguments) (cdr arguments)))
      (arguments func) '()
      env))
    
    ((eql 'funcall (operator func))
     (evaluate/cps-funcall (arguments func) env k))
    
    ((eql 'apply (operator func))
     (evaluate/cps-apply   (arguments func) '() env k))
    
    ((and (symbolp (operator func))
          (get (operator func) 'defun/cc))
     (evaluate-arguments-then-apply
      (lambda (arguments)
        (apply-cps-lambda (get (operator func) 'defun/cc) arguments k))
      (arguments func) '()
      env))

    ((and (symbolp (operator func))
          (get (operator func) 'defmethod/cc))
     (evaluate-arguments-then-apply
      (lambda (arguments)
        (apply-cps-lambda (apply (fdefinition (operator func)) arguments) arguments k))
      (arguments func) '()
      env))
       
    (t
     (evaluate-arguments-then-apply
      (lambda (arguments)
        (apply #'kontinue k (multiple-value-list (apply (fdefinition (operator func)) arguments))))
      (arguments func) '()
      env))))

(defmethod evaluate/cps ((func local-application-form) env k)
  (evaluate/cps-apply (arguments func) (list (list (lookup env :flet (operator func) :error-p t))) env k))

(defk k-for-cps-apply (remaining-arguments evaluated-arguments env k)
    (value)
  (evaluate/cps-apply (cdr remaining-arguments) (cons value evaluated-arguments)
                      env k))

(defun evaluate/cps-apply (remaining-arguments evaluated-arguments env k)
  (if remaining-arguments
      (evaluate/cps (car remaining-arguments) env
                    `(k-for-cps-apply ,remaining-arguments ,evaluated-arguments ,env ,k))
      (let ((arg-list (apply #'list* (reverse evaluated-arguments))))
        (apply-cps-lambda (first arg-list) (rest arg-list) k))))

(defun evaluate/cps-funcall (arguments env k)
  (evaluate/cps-apply (append (butlast arguments)
                              (list (make-instance 'free-application-form
                                                   :operator 'list
                                                   :source `(list ,(source (car (last arguments))))
                                                   :arguments (last arguments))))
                      '()
                      env k))

(defmethod apply-cps-lambda ((operator cps-closure) effective-arguments k)
  (let ((env (env operator))
        (remaining-arguments effective-arguments)
        (remaining-parameters (arguments (code operator))))
    ;; in this code ARGUMENT refers to the values passed to the
    ;; function. PARAMETER refers to the lambda of the closure
    ;; object. we walk down the parameters and put the arguments in
    ;; the environment under the proper names.

    ;; first the required arguments
    (block required-parameters
      (loop
         while remaining-parameters
         for parameter = (first remaining-parameters)
         do (typecase parameter
              (required-function-argument-form
               (if remaining-arguments
                   (setf env (register env :let (name parameter) (pop remaining-arguments)))
                   (error "Missing required arguments, expected ~S, got ~S."
                          (arguments (code operator)) effective-arguments))
               (pop remaining-parameters))
              (t (return-from required-parameters t)))))
    
    ;; now the optional arguments (nb: both remaining-arguments and
    ;; remaining-parameters have been modified)
    (dolist (parameter remaining-parameters)
      (typecase parameter
        (optional-function-argument-form
         (if remaining-arguments
             (progn
               (setf env (register env :let (name parameter) (pop remaining-arguments)))
               (when (supplied-p-parameter parameter)
                 (setf env (register env :let (supplied-p-parameter parameter) t))))
             (return-from apply-cps-lambda
               ;; we need to evaluate a default-value, since this may
               ;; contain call/cc we need to setup the continuation
               ;; and let things go from there (hence the return-from)
               (evaluate/cps (default-value parameter) env
                             `(k-for-apply-cps/optional-argument-default-value
                               ;; remaining-arguments is, by
                               ;; definition, NIL so we needn't pass
                               ;; it here.
                               ,operator ,remaining-parameters ,env ,k)))))))
    
    (evaluate/cps-progn (body (code operator)) env k)))

(defk k-for-apply-cps/optional-argument-default-value
    (operator remaining-parameters env k)
    (value)
  (apply-cps-lambda/optional-default-value
   operator (cdr remaining-parameters)
   (register env :let (name (first remaining-parameters)) value) k))

(defun apply-cps-lambda/optional-default-value (operator remaining-parameters env k)
  (if remaining-parameters
      (dolist (parameter remaining-parameters)
        (typecase parameter
          (optional-function-argument-form
           (return-from apply-cps-lambda/optional-default-value
             (evaluate/cps (default-value parameter) env
                           `(k-for-apply-cps/optional-argument-default-value
                             ,operator ,(cdr remaining-parameters) ,env ,k))))
          (t (error "Sorry, only required and optional arguments for now."))))
      (evaluate/cps-progn (body (code operator)) env k)))

(defmethod apply-cps-lambda ((operator function) effective-arguments k)
  (apply #'kontinue k (multiple-value-list (apply operator effective-arguments))))

(defk k-for-evaluate-arguments-then-apply (handler remaining-arguments evaluated-arguments env)
    (value)
  (evaluate-arguments-then-apply
   handler
   remaining-arguments (cons value evaluated-arguments)
   env))

(defun evaluate-arguments-then-apply (handler remaining-arguments evaluated-arguments env)
  (if remaining-arguments
      (evaluate/cps (car remaining-arguments) env
                    `(k-for-evaluate-arguments-then-apply ,handler ,(cdr remaining-arguments)
                                                          ,evaluated-arguments ,env))
      (funcall handler (reverse evaluated-arguments))))

;;;; FUNCTION and LAMBDA application

(defmethod evaluate/cps ((lambda lambda-application-form) env k)
  (evaluate/cps-funcall (cons (operator lambda) (arguments lambda)) env k))

;;;; Constants

(defmethod evaluate/cps ((c constant-form) env k)
  (declare (ignore env))
  (kontinue k (value c)))

;;;; BLOCK/RETURN-FROM

(defmethod evaluate/cps ((block block-form) env k)
  (evaluate/cps-progn (body block) (register env :block (name block) k) k))

(defmethod evaluate/cps ((return return-from-form) env k)
  (declare (ignore k))
  (evaluate/cps (result return) env (lookup env :block (name (target-block return)) :error-p t)))

;;;; CATCH/THROW

(defk catch-tag-k (catch env k)
    (tag)
  (evaluate/cps-progn (body catch) (register env :catch tag k) k))

(defmethod evaluate/cps ((catch catch-form) env k)
  (evaluate/cps (tag catch) env `(catch-tag-k ,catch ,env ,k)))

(defk throw-tag-k (throw env k)
    (tag)
  (evaluate/cps (value throw) env (lookup env :catch tag :error-p t)))

(defmethod evaluate/cps ((throw throw-form) env k)
  (evaluate/cps (tag throw) env
                `(throw-tag-k ,throw ,env ,k)))

;;;; FLET/LABELS

(defmethod evaluate/cps ((flet flet-form) env k)
  (let ((new-env env))
    (dolist* ((name . form) (binds flet))
      (setf new-env (register new-env :flet name (make-instance 'cps-closure
                                                                :code form
                                                                :env env))))
    (evaluate/cps-progn (body flet) new-env k)))

(defmethod evaluate/cps ((labels labels-form) env k)
  (let ((closures '()))
    (dolist* ((name . form) (binds labels))
      (let ((closure (make-instance 'cps-closure :code form)))
        (setf env (register env :flet name closure))
        (push closure closures)))
    (dolist (closure closures)
      (setf (env closure) env))
    (evaluate/cps-progn (body labels) env k)))

;;;; LET/LET*

(defmethod evaluate/cps ((let let-form) env k)
  (evaluate/cps-let (binds let) nil (body let) env k))

(defk k-for-evaluate/cps-let (var remaining-bindings evaluated-bindings body env k)
    (value)
  (evaluate/cps-let remaining-bindings
                    (cons (cons var value) evaluated-bindings)
                    body env k))

(defun evaluate/cps-let (remaining-bindings evaluated-bindings body env k)
  (if remaining-bindings
      (destructuring-bind (var . initial-value)
          (car remaining-bindings)
        (evaluate/cps initial-value env
                      `(k-for-evaluate/cps-let
                        ,var
                        ,(cdr remaining-bindings)
                        ,evaluated-bindings
                        ,body
                        ,env
                        ,k)))
      (dolist* ((var . value) evaluated-bindings
                (evaluate/cps-progn body env k))
        (setf env (register env :let var value)))))

(defmethod evaluate/cps ((let* let*-form) env k)
  (evaluate/cps-let* (binds let*) (body let*) env k))

(defk k-for-evaluate/cps-let* (var bindings body env k)
    (value)
  (evaluate/cps-let* bindings body
                     (register env :let var value) k))

(defun evaluate/cps-let* (bindings body env k)
  (if bindings
      (destructuring-bind (var . initial-value)
          (car bindings)
        (evaluate/cps initial-value env
                      `(k-for-evaluate/cps-let* ,var ,(cdr bindings) ,body ,env ,k)))
      (evaluate/cps-progn body env k)))

;;;; IF

(defk k-for-evaluate/cps-if (then else env k)
    (value)
  (if value
      (evaluate/cps then env k)
      (evaluate/cps else env k)))

(defmethod evaluate/cps ((if if-form) env k)
  (evaluate/cps (consequent if) env
                `(k-for-evaluate/cps-if ,(then if) ,(else if) ,env ,k )))

;;;; LOCALLY

(defmethod evaluate/cps ((locally locally-form) env k)
  (evaluate/cps-progn (body locally) env k))

;;;; MACROLET

(defmethod evaluate/cps ((macrolet macrolet-form) env k)
  (evaluate/cps-progn (body macrolet) env k))

;;;; multiple-value-call

(defk k-for-m-v-c (remaining-arguments evaluated-arguments env k)
    (value other-values)
  (evaluate-m-v-c
   remaining-arguments (append evaluated-arguments (list value) other-values)
   env k))

(defun evaluate-m-v-c (remaining-arguments evaluated-arguments env k)
  (if remaining-arguments
      (evaluate/cps (car remaining-arguments) env
                    `(k-for-m-v-c  ,(cdr remaining-arguments) ,evaluated-arguments ,env ,k))
      (destructuring-bind (function &rest arguments)
          evaluated-arguments
        (etypecase function
          (cps-closure (apply-cps-lambda function evaluated-arguments k))
          (function (apply #'kontinue k (multiple-value-list
                                         (multiple-value-call function (values-list arguments)))))))))

(defmethod evaluate/cps ((m-v-c multiple-value-call-form) env k)
  (evaluate-m-v-c (list* (func m-v-c) (arguments m-v-c)) '() env k))

;;;; PROGN

(defmethod evaluate/cps ((progn progn-form) env k)
  (evaluate/cps-progn (body progn) env k))

(defk k-for-evaluate/cps-progn (rest-of-body env k)
    ()
  (evaluate/cps-progn rest-of-body env k))

(defun evaluate/cps-progn (body env k)
  (cond
    ((cdr body)
      (evaluate/cps (first body) env
                    `(k-for-evaluate/cps-progn ,(cdr body) ,env ,k)))
    (body
     (evaluate/cps (first body) env k))
    (t
     (kontinue k nil))))

;;;; SETQ

(defk k-for-local-setq (var env k)
    (value)
  (setf (lookup env :let var :error-p t) value)
  (kontinue k value))

(defk k-for-free-setq (var env k)
    (value)
  (setf (symbol-value var) value)
  (kontinue k value))

(defk k-for-local-lexical-setq (var env k)
    (value)
  (funcall (second (lookup env :lexical-let var :error-p t)) value)
  (kontinue k value))

(defmethod evaluate/cps ((setq setq-form) env k)
  (multiple-value-bind (value foundp)
      (lookup env :let (var setq))
    (declare (ignore value))
    (if foundp
        (evaluate/cps (value setq)
                      env `(k-for-local-setq ,(var setq) ,env ,k))
        (multiple-value-bind (value foundp)
            (lookup env :lexical-let (var setq))
          (declare (ignore value))
          (if foundp
              (evaluate/cps (value setq)
                            env `(k-for-local-lexical-setq ,(var setq) ,env ,k))
              (evaluate/cps (value setq)
                            env `(k-for-free-setq ,(var setq) ,env ,k)))))))

;;;; SYMBOL-MACROLET

(defmethod evaluate/cps ((symbol-macrolet symbol-macrolet-form) env k)
  (evaluate/cps-progn (body symbol-macrolet) env k))

;;;; TAGBODY/GO

(defk tagbody-k (k)
    ()
  (kontinue k nil))

(defmethod evaluate/cps ((tagbody tagbody-form) env k)
  (evaluate/cps-progn (body tagbody) (register env :tag tagbody k) `(tagbody-k ,k)))

(defmethod evaluate/cps ((go-tag go-tag-form) env k)
  (declare (ignore go-tag env))
  (kontinue k nil))

(defmethod evaluate/cps ((go go-form) env k)
  (declare (ignore k))
  (evaluate/cps-progn (target-progn go) env (lookup env :tag (enclosing-tagbody go) :error-p t)))

;;;; THE

(defmethod evaluate/cps ((the the-form) env k)
  (evaluate/cps (value the) env k))

;;;; DEFUN/CC and DEFMETHOD/CC

(defun extract-argument-names (lambda-list &key allow-specializers keep-lambda-keywords)
  (let ((state :required)
        (argument-names '()))
    (loop
       for argument in lambda-list
       do (if (member argument '(&optional &key &rest &allow-other-keys))
              (progn
                (setf state argument)
                (when keep-lambda-keywords
                  (push argument argument-names)))
              (ecase state
                (:required
                 (push (if allow-specializers
                           (if (consp argument)
                               (first argument)
                               argument)
                           argument)
                       argument-names))
                (&optional (push (if (consp argument)
                                     (first argument)
                                     argument)
                                 argument-names))
                (&key (push (if (consp argument)
                                (if (consp (first argument))
                                    (second (first argument))
                                    (first argument))
                                argument)
                            argument-names))
                (&allow-other-keys
                 (when keep-lambda-keywords
                   (push argument argument-names)))
                (&rest (push argument argument-names)))))
    (nreverse argument-names)))

(defmacro defun/cc (name arguments &body body)
  `(progn
     (setf (get ',name 'defun/cc) (make-instance 'cps-closure
                                                 :code (walk-form '(lambda ,arguments ,@body) nil nil)
                                                 :env nil))
     (defun ,name ,arguments
       (declare (ignorable ,@(extract-argument-names arguments :allow-specializers nil)))
       (error "Sorry, /CC function are not callable outside of with-call/cc."))))

(defmacro defmethod/cc (name arguments &body body)
  `(progn
     (setf (get ',name 'defmethod/cc) t)
     (defmethod ,name ,arguments
       (declare (ignorable ,@(extract-argument-names arguments :allow-specializers t)))
       (make-instance 'cps-closure
                      :code (walk-form '(lambda ,(extract-argument-names arguments :allow-specializers t
                                                                         :keep-lambda-keywords t)
                                         ,@body)
                                       nil nil)
                      :env nil))))

(defmacro defgeneric/cc (name args &rest options)
  "Trivial wrapper around defgeneric designed to alert readers that these methods are cc methods."
  `(defgeneric ,name ,args ,@options))
