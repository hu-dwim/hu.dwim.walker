;; -*- lisp -*-

(in-package :it.bese.arnesi)

;;;; * A Code Walker

;;;; ** Public Entry Point

(defun walk-form (form &optional (parent nil) (env (make-walk-env)))
  "Walk FORM and return a FORM object."
  (funcall (find-walker-handler form) form parent env))

(defun make-walk-env (&optional lexical-env)
  (let ((walk-env '()))
    (when lexical-env
      (dolist (var (lexical-variables lexical-env))
        (setf walk-env (register walk-env :lexical-let var t))))
    walk-env))

;;;; This takes a Common Lisp form and transforms it into a tree of
;;;; FORM objects.

(defvar *walker-handlers* (make-hash-table :test 'eq))

(defun find-walker-handler (form)
  "Simple function which tells us what handler should deal
  with FORM. Signals an error if we don't have a handler for
  FORM."
  (if (atom form)
      (gethash 'atom *walker-handlers*)
      (case (car form)
        ((block declare flet function go if labels let let*
          macrolet progn quote return-from setq symbol-macrolet
          tagbody unwind-protect catch multiple-value-call
          multiple-value-prog1 throw load-time-value the
          eval-when locally progv)
         (or
          (gethash (car form) *walker-handlers*)
          (error "Sorry, No walker for the special operater ~S defined." (car form))))
        (t (gethash 'application *walker-handlers*)))))

(defmacro defwalker-handler (name (form parent lexical-env)
                             &body body)
  `(progn
     (setf (gethash ',name *walker-handlers*)
           (lambda (,form ,parent ,lexical-env)
             (declare (ignorable ,parent ,lexical-env))
             ,@body))
     ',name))

(defclass form ()
  ((parent :accessor parent :initarg :parent)
   (source :accessor source :initarg :source)))

(defmethod make-load-form ((object form) &optional env)
  (let ((slot-names '()))
    (labels ((push-direct-slots (class)
               (dolist (slot (mopp:class-slots class))
                 (push (mopp:slot-definition-name slot) slot-names)))
             (collect-slot-names (class)
               (push-direct-slots class)
               (unless (eql class (find-class 'form))
                 (dolist (klass (mopp:class-direct-superclasses class))
                   (collect-slot-names klass)))))
      (collect-slot-names (class-of object))
      (setf slot-names (remove-duplicates slot-names :test #'eql))
      (make-load-form-saving-slots object
                                   :slot-names slot-names
                                   :environment env))))

(defmethod print-object ((form form) stream)
  (print-unreadable-object (form stream :type t :identity t)
    (when (slot-boundp form 'source)
      (let ((*print-readably* nil)
            (*print-level* 0)
            (*print-length* 4))
        (format stream "~S" (source form))))))

(defun register (environment type name datum &rest other-datum)
  (cons (if other-datum
            (list* type name datum other-datum)
            (list* type name datum))
        environment))

(defun lookup (environment type name &key (error-p nil))
  (loop
     for (.type .name . data) in environment
     when (and (eql .type type) (eql .name name))
       return data
     finally
       (when error-p
         (error "Sorry, No value for ~S of type ~S in environment ~S found."
                name type environment))))

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

(defmacro with-form-object ((variable type &rest initargs)
                            &body body)
  `(let ((,variable (make-instance ',type ,@initargs)))
     ,@body
     ,variable))

(defclass implicit-progn-mixin ()
  ((body :accessor body :initarg :body)))

(defclass binding-form-mixin ()
  ((binds :accessor binds :initarg :binds)))

(defun split-body (body env &key (docstring t) (declare t))
  (let ((documentation nil))
    (flet ((done ()
             (return-from split-body (values body env documentation))))
      (loop
         for form = (car body)
         while body
         do (typecase form
              (cons (if (and declare (eql 'cl:declare (first form)))
                        ;; declare form
                        (let ((declarations (rest form)))
                          (dolist* (dec declarations)
                            (setf env (parse-declaration dec env))))
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

(defun parse-declaration (declaration environment)
  (macrolet ((extend-env ((var list) &rest datum)
               `(dolist (,var ,list)
                  (setf environment (register environment :declare ,@datum)))))
    (destructuring-bind (type &rest arguments)
        declaration
      (case type
        (dynamic-extent
         (extend-env (var arguments)
                     var `(dynamic-extent)))        
        (ftype
         (extend-env (function-name (cdr arguments))
                     function-name `(ftype ,(first arguments))))
        (ignorable
         (extend-env (var arguments)
                     var `(ignorable)))
        (ignore
         (extend-env (var arguments)
                     var `(ignorable)))
        (inline
          (extend-env (function arguments) function `(ignorable)))
        (notinline
         (extend-env (function arguments) function `(notinline)))
        (optimize
         (extend-env (optimize-spec arguments) 'optimize optimize-spec))
        (special
         (extend-env (var arguments) var `(special)))
        (type
         (extend-env (var (rest arguments)) var `(type ,(first arguments))))
        (t
         (extend-env (var arguments) var `(type ,type))))))
  environment)

(defun walk-implict-progn (parent forms env &key docstring declare)
  (multiple-value-bind (body env docstring)
      (split-body forms env :docstring docstring :declare declare)
    (values (mapcar (lambda (form)
                      (walk-form form parent env))
                    body)
            docstring)))

;;;; Atoms

(defclass constant-form (form)
  ((value :accessor value :initarg :value)))

(defclass variable-reference (form)
  ((name :accessor name :initarg :name)))

(defmethod print-object ((v variable-reference) stream)
  (print-unreadable-object (v stream :type t :identity t)
    (format stream "~S" (name v))))

(defclass local-variable-reference (variable-reference)
  ())

(defclass local-lexical-variable-reference (local-variable-reference)
  ()
  (:documentation "A reference to a local variable defined in the
  lexical envorinment outside of the form passed to walk-form."))

(defclass free-variable-reference (variable-reference)
  ())

(defwalker-handler atom (form parent env)
  (declare (special *macroexpand*))
  (cond
    ((constantp form)
     (make-instance 'constant-form :value form
                    :parent parent :source form))
    ((lookup env :let form)
     (make-instance 'local-variable-reference :name form
                    :parent parent :source form))
    ((lookup env :lexical-let form)
     (make-instance 'local-lexical-variable-reference :name form
                    :parent parent :source form))
    ((lookup env :symbol-macrolet form)
     (walk-form (lookup env :symbol-macrolet form) parent env))
    (t
     (make-instance 'free-variable-reference :name form
                    :parent parent :source form))))

;;;; Function Applictation

(defclass application-form (form)
  ((operator :accessor operator :initarg :operator)
   (arguments :accessor arguments :initarg :arguments)))

(defclass local-application-form (application-form)
  ((code :accessor code :initarg :code)))

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
        (with-form-object (application lambda-application-form :parent parent :source form)
          (setf (operator application) (walk-form op application env)
                (arguments application) (mapcar (lambda (form)
                                                  (walk-form form application env))
                                                args))
          (return application)))
      (when (lookup env :macrolet op)
        (return (walk-form (apply (lookup env :macrolet op) args) parent env)))
      (when (macro-function op)
         (multiple-value-bind (expansion expanded)
             (macroexpand-1 form nil)
           (when expanded
             (return (walk-form expansion parent env)))))
      (let ((app (if (lookup env :flet op)
                     (make-instance 'local-application-form :code (lookup env :flet op))
                     (make-instance 'free-application-form))))
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

(defclass lambda-function-form (function-form implicit-progn-mixin)
  ((arguments :accessor arguments :initarg :arguments)))

(defclass function-object-form (form)
  ((name :accessor name :initarg :name)))

(defclass local-function-object-form (function-object-form)
  ())

(defclass free-function-object-form (function-object-form)
  ())

(defwalker-handler function (form parent env)
  (if (symbolp (second form))
      ;; (function foo)
      (make-instance (if (lookup env :flet (second form))
                         'local-function-object-form
                         'free-function-object-form)
                     :name (second form)
                     :parent parent :source form)
      ;; (function (lambda ...))
      (walk-lambda (second form) parent env)))

(defun walk-lambda (form parent env)
  (with-form-object (func lambda-function-form
                          :parent parent
                          :source form)
    (let ((arguments '())
          (state))
      ;; 1) parse the argument list creating a list of FUNCTION-ARGUMENT-FORM objects
      (setf state :required)
      (dolist (argument (second form))
        (flet ((extend-env (argument)
                 (setf env (register env :let (name argument) argument))))
          (if (member argument '(&optional &key &rest))
              (setf state argument)
              (case state
                (:required
                 (push (walk-required-argument argument func env) arguments)
                 (extend-env (car arguments)))
                (&optional
                 (push (walk-optional-argument argument func env) arguments)
                 (extend-env (car arguments)))
                (&key
                 (if (eql argument '&allow-other-keys)
                     (push (make-instance 'allow-other-keys-function-argument
                                          :parent func :source argument)
                           arguments)
                     (push (walk-keyword-argument argument func env) arguments))
                 (extend-env (car arguments)))
                (&rest
                 (push (walk-rest-argument argument func env) arguments)
                 (extend-env (car arguments)))))))
      (setf (arguments func) (nreverse arguments))
      ;; 2) parse the body
      (setf (body func) (walk-implict-progn func (cddr form) env :declare t))
      ;; all done
      func)))

(defclass function-argument-form (form)
  ((name :accessor name :initarg :name)))

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

(defclass keyword-function-argument-form (optional-function-argument-form)
  ((keyword-name :accessor keyword-name :initarg :keyword-name)))

(defun walk-keyword-argument (form parent env)
  (destructuring-bind (name &optional default-value supplied-p-parameter)
      (ensure-list form)
    (let ((name (if (consp name)
                    (second name)
                    name))
          (keyword (if (consp name)
                       (first name)
                       (intern (string name) :keyword))))
      (with-form-object (arg keyword-function-argument-form
                             :parent parent
                             :source form
                             :name name
                             :keyword-name keyword
                             :supplied-p-parameter supplied-p-parameter)
        (setf (default-value arg) (walk-form default-value arg env))))))

(defclass allow-other-keys-function-argument-form (function-argument-form)
  ())

(defclass rest-function-argument-form (optional-function-argument-form)
  ())

(defun walk-rest-argument (form parent env)
  (declare (ignore env))
  (make-instance 'rest-function-argument-form :name form
                 :parent parent :source form))

;;;; Block/Return From

(defclass block-form (form implicit-progn-mixin)
  ((name :accessor name :initarg :name)))

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
                                             (register env :block block-name block))))))

(defwalker-handler return-from (form parent env)
  (destructuring-bind (block-name &optional (value '(values)))
      (cdr form)
    (if (lookup env :block block-name)
        (with-form-object (return-from return-from-form :parent parent :source form
                           :target-block (lookup env :block block-name))
          (setf (result return-from) (walk-form value return-from env)))
        (error "Can't return-from unknown block named ~S." block-name))))

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
  (declare (ignore form parent env))
  (error "Sorry, EVAL-WHEN not yet implemented."))

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

(defclass function-binding-form (form binding-form-mixin implicit-progn-mixin)
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
      (setf (body flet) (walk-implict-progn flet
                                            body
                                            (loop
                                               with env = env
                                               for (name . lambda) in (binds flet)
                                               do (setf env (register env :flet name lambda))
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
         do (setf env (register env :flet name lambda)))
      (setf (binds labels) (nreverse (binds labels)))
      (loop
         for form in binds
         for (arguments . body) = (cdr form)
         for binding in (binds labels)
         for lambda = (cdr binding)
         for tmp-lambda = (walk-lambda `(lambda ,arguments ,@body) labels env)
         do (setf (body lambda) (body tmp-lambda)
                  (arguments lambda) (arguments tmp-lambda)))
      (setf (body labels) (walk-implict-progn labels body env)))))

;;;; LET/LET*

(defclass variable-binding-form (form binding-form-mixin implicit-progn-mixin)
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
    (dolist* ((var . value) (binds let))
      (declare (ignore value))
      (setf env (register env :let var :dummy)))
    (setf (body let) (walk-implict-progn let (cddr form) env :declare t))))

(defclass let*-form (variable-binding-form)
  ())

(defwalker-handler let* (form parent env)
  (with-form-object (let* let*-form :parent parent :source form :binds '())
    (dolist* ((var &optional initial-value) (mapcar #'ensure-list (second form)))
      (push (cons var (walk-form initial-value let* env)) (binds let*))
      (setf env (register env :let var :dummy)))
    (setf (binds let*) (nreverse (binds let*))
          (body let*) (walk-implict-progn let* (cddr form) env :declare t))))

;;;; LOCALLY

(defclass locally-form (form implicit-progn-mixin)
  ())

(defwalker-handler locally (form parent env)
  (with-form-object (locally locally-form :parent parent :source form)
    (setf (body locally) (walk-implict-progn locally (cdr form) env :declare t))))

;;;; MACROLET

(defclass macrolet-form (form binding-form-mixin implicit-progn-mixin)
  ())

(defwalker-handler macrolet (form parent env)
  (with-form-object (macrolet macrolet-form :parent parent :source form
                              :binds '())
    (dolist* ((name args &body body) (second form))
      (let ((handler (eval `(lambda ,args ,@body))))
        (setf env (register env :macrolet name handler))
        (push (cons name handler) (binds macrolet))))
    (setf (binds macrolet) (nreverse (binds macrolet))
          (body macrolet) (walk-implict-progn macrolet (cddr form) env :declare t))))

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

;;;; QUOTE

(defwalker-handler quote (form parent env)
  (make-instance 'constant-form :parent parent :source form :value (second form)))

;;;; SETQ

(defclass setq-form (form)
  ((var   :accessor var   :initarg :var)
   (value :accessor value :initarg :value)))

(defwalker-handler setq (form parent env)
  ;; the SETQ handler needs to be able to deal with symbol-macrolets
  ;; which haven't yet been expanded and may expand into something
  ;; requiring setf and not setq.
  (let ((effective-code '()))
    (loop
       for (name value) on (cdr form) by #'cddr
       if (lookup env :symbol-macrolet name)
         do (push `(setf ,(lookup env :symbol-macrolet name) ,value) effective-code)
       else
         do (push `(setq ,name ,value) effective-code))
    (if (= 1 (length effective-code))
        ;; only one form, the "simple case"
        (destructuring-bind (type var value)
            (first effective-code)
          (ecase type
            (setq (with-form-object (setq setq-form :parent parent :source form
                                          :var var)
                    (setf (value setq) (walk-form value setq env))))
            (setf (walk-form (first effective-code) parent env))))
        ;; multiple forms
        (with-form-object (progn progn-form :parent parent :source form)
          (setf (body progn) (walk-implict-progn progn effective-code env))))))

;;;; SYMBOL-MACROLET

(defclass symbol-macrolet-form (form binding-form-mixin implicit-progn-mixin)
  ())

(defwalker-handler symbol-macrolet (form parent env)
  (with-form-object (symbol-macrolet symbol-macrolet-form :parent parent :source form
                                     :binds '())
    (dolist* ((symbol expansion) (second form))
      (setf env (register env :symbol-macrolet symbol expansion))
      (push (cons symbol expansion) (binds symbol-macrolet)))
    (setf (binds symbol-macrolet) (nreverse (binds symbol-macrolet))
          (body symbol-macrolet) (walk-implict-progn parent (cddr form) env))))

;;;; TAGBODY/GO

(defclass tagbody-form (form implicit-progn-mixin)
  ())

(defclass go-tag-form (form)
  ((name :accessor name :initarg :name)))

(defgeneric go-tag-form-p (object)
  (:method ((object go-tag-form)) t)
  (:method ((object t))           nil))

(defwalker-handler tagbody (form parent env)
  (with-form-object (tagbody tagbody-form :parent parent :source form :body (cdr form))
    (setf env (register env :tagbody 'enclosing-tagbody tagbody))
    (flet ((go-tag-p (form)
             (or (symbolp form) (integerp form))))
      ;; the loop below destructuivly modifies the body of tagbody,
      ;; since it's the same object as the source we need to copy it.
      (setf (body tagbody) (copy-list (body tagbody)))
      (loop
         for part on (body tagbody)
         if (go-tag-p (car part))
           do (setf env (register env :tag (car part) (cdr part))))
      (loop
         for part on (body tagbody)
         if (go-tag-p (car part))
           do (setf (car part) (make-instance 'go-tag-form :parent tagbody
                                              :source (car part)
                                              :name (car part)))
         else
           do (setf (car part) (walk-form (car part) tagbody env))))))

(defclass go-form (form)
  ((target-progn :accessor target-progn :initarg :target-progn)
   (name :accessor name :initarg :name)
   (enclosing-tagbody :accessor enclosing-tagbody :initarg :enclosing-tagbody)))

(defwalker-handler go (form parent env)
  (make-instance 'go-form
                 :parent parent
                 :source form
                 :name (second form)
                 :target-progn (lookup env :tag (second form))
                 :enclosing-tagbody (lookup env :tagbody 'enclosing-tagbody)))
