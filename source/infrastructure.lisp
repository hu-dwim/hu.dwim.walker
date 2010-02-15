;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.walker)

(def structure (walk-environment (:constructor %make-walk-environment (&optional walked-environment lexical-environment))
                                 (:conc-name #:env/))
  ;; TODO the walked-environment slot name and is confusing. either rename the struct, or rename this slot to something more intuitive...
  (walked-environment '()) ; contains the already walked *-form instances
  lexical-environment) ; the underlying lisp's internal lexenv

(def (condition* e) walker-error (error)
  ())

(def (condition* e) simple-walker-error (simple-error walker-error)
  ())

(def function simple-walker-error (message &rest args)
  (error 'simple-walker-error :format-control message :format-arguments args))

(def (function e) macroexpand-all (form &optional (env (make-empty-lexical-environment)))
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
  (:method :around (form &key parent environment)
    (bind ((*current-form* (or *current-form*
                               form))
           (work-env (or environment (make-walk-environment))))
      (with-current-form form
        (call-next-layered-method form :parent parent :environment work-env)))))

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

(def (function e) special-variable-name? (name &optional lexenv)
  "Determines if the name has been globally proclaimed special."
  (and (symbolp name)
       (not (keywordp name))
       (not (member name '(t nil) :test #'eq))
       (or (boundp name)
           (proclaimed-special-in-lexenv? name lexenv)
           ;; This is the only portable way to check if a symbol is declared special, without being boundp, i.e. (defvar 'foo). Maybe we should make it optional with a compile-time flag?
           #+nil
           (eval `((lambda ()
                     (flet ((func ()
                              (symbol-value ',var)))
                       (let ((,var t))
                         (declare (ignorable ,var))
                         (ignore-errors (func))))))))))

(def (function e) collect-standard-walked-form-subclasses ()
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
  (:method :in ignore-undefined-references :around (type name)
    ;; well, we ignore them in this layer...
    ))

;;;
;;; Walk environment
;;;

(def (function e) make-walk-environment (&optional lexenv)
  (unless lexenv
    (setf lexenv (make-empty-lexical-environment)))
  (bind ((walkedenv '()))
    (macrolet ((extend (type name datum &rest other-datum)
                 `(setf walkedenv (%repository/augment walkedenv ,type ,name ,datum ,@other-datum))))
      (do-variables-in-lexenv (lexenv name ignored? special? macro? macro-body type)
        (if macro?
            (extend :symbol-macro name macro-body)
            (extend :unwalked-variable name
                    (cons (cond (special? :special)
                                (ignored? :ignored)
                                (t t))
                          type))))
      (do-functions-in-lexenv (lexenv name macro? macro-fn)
        (if macro?
            (extend :macro name macro-fn)
            (extend :unwalked-function name t))))
    (%make-walk-environment (nreverse walkedenv) lexenv)))

(def function augment-walk-environment (env type name &optional datum)
  (bind ((walkenv (env/walked-environment env))
         (lexenv (env/lexical-environment env)))
    (setf walkenv (%repository/augment walkenv type name datum))
    (setf lexenv (ecase type
                   (:variable     (augment-lexenv-with-variable     name lexenv))
                   (:macro        (augment-lexenv-with-macro        name datum lexenv))
                   (:function     (augment-lexenv-with-function     name lexenv))
                   (:symbol-macro (augment-lexenv-with-symbol-macro name datum lexenv))
                   (:block        (augment-lexenv-with-block        name lexenv))
                   (:tag          (augment-lexenv-with-tag          name lexenv))
                   (:unwalked-variable
                    (assert (eql (car datum) :special))
                    (augment-lexenv-with-variable name lexenv :special t))
                   ;; TODO
                   (:declare      lexenv)
                   (:tagbody      lexenv)))
    (%make-walk-environment walkenv lexenv)))

(defmacro augment-walk-environment! (env type name datum &rest other-datum)
  `(setf ,env (augment-walk-environment ,env ,type ,name ,datum ,@other-datum)))

(def function %repository/augment (environment type name datum &rest other-datum)
  (cons (if other-datum
            (list* type name datum other-datum)
            (list* type name datum))
        environment))

(def function %repository/find (environment type name &key (otherwise nil))
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

#+nil ;; it's not used for now
(defun (setf %repository/find) (value environment type name &key (error-p nil))
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

(def (condition* e) walker-warning (warning)
  ((enclosing-code *current-form*)))

(def (condition* e) walker-style-warning (walker-warning
                                          style-warning)
  ())

(def (condition* e) simple-walker-style-warning (walker-style-warning
                                                 simple-style-warning)
  ())

(def function simple-walker-style-warning (format-control &rest format-arguments)
  (warn 'simple-walker-style-warning :format-control format-control :format-arguments format-arguments))

(def (condition* e) undefined-reference (walker-style-warning)
  ((name)))

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

(def function find-walker-handler (form-name)
  (or (gethash form-name *walker-handlers*)
      (case form-name
        ((block declare flet function go if labels let let*
                macrolet progn quote return-from setq symbol-macrolet
                tagbody unwind-protect catch multiple-value-call
                multiple-value-prog1 throw load-time-value the
                eval-when locally progv)
         (error "Sorry, no walker for the special operator ~S defined." form-name))
        (t (gethash 'application *walker-handlers*)))))

(def (layered-function e) walk-compound-form (name form parent environment)
  (:documentation "Dispatches to a form-specific walker using the name")
  (:method ((name t) form parent environment)
    (funcall (find-walker-handler name) form parent environment)))

(def layered-method walk-form ((form cons) &key parent environment)
  (walk-compound-form (car form) form parent environment))

(def layered-method walk-form ((form t) &key parent environment)
  (funcall (find-walker-handler +atom-marker+) form parent environment))

(def function walker-handler-definition (name &optional (table *walker-handlers*))
  (gethash name table))

(def function (setf walker-handler-definition) (handler name &optional (table *walker-handlers*))
  (when (gethash name table)
    (simple-style-warning "Redefining walker handler for ~S" name))
  (setf (gethash name table) handler))

(def function %walker-handler-symbol (name)
  (format-symbol *package* "WALKER-HANDLER/~A" name))

(def function %walker-handler-body (body)
  `(block nil
     (bind ((-form- (coerce-to-form -form-)))
       (macrolet ((-lookup- (type name &key (otherwise nil))
                    `(%repository/find (env/walked-environment -environment-) ,type ,name :otherwise ,otherwise))
                  (-augment- (type name &optional datum)
                    `(setf -environment- (augment-walk-environment -environment- ,type ,name ,datum))))
         (flet ((recurse (node &optional (parent -parent-) (environment -environment-))
                  (walk-form node :parent parent :environment environment)))
           (declare (ignorable #'recurse))
           (with-current-form -form-
             ,@body))))))

(def function %defwalker-handler-body (name body &optional declarations)
  (bind ((function-name (%walker-handler-symbol name)))
    `(progn
       (defun ,function-name (-form- -parent- -environment-)
         (declare (ignorable -parent- -environment-)
                  ,@declarations)
         ,(%walker-handler-body body))
       (setf (walker-handler-definition ',name) ',function-name)
       ',name)))

(def (macro e) defwalker-handler (name &body body)
  (%defwalker-handler-body name body))

(def (definer e :available-flags "od") walker (name &body body)
  (with-standard-definer-options name
    (%defwalker-handler-body name body (function-like-definer-declarations -options-))))

(def function layered-method-qualifiers (options)
  (flatten (list
            (awhen (or (getf options :in-layer)
                       (getf options :in))
              (list :in it))
            (getf options :mode))))

(def (definer e :available-flags "od") walker-method (name &body body)
  (let ((qualifiers (layered-method-qualifiers -options-))
        (type-match? nil))
    (when (consp name)
      (setf type-match? t)
      (when (eql (first name) 'type)
        (setf name (second name))))
    `(progn
       (define-layered-method ,(if type-match? 'walk-form 'walk-compound-form)
         ,@qualifiers ,(if type-match?
                           `((-form- ,name) &key
                             ((:parent -parent-)) ((:environment -environment-)))
                           `((-name- (eql ',name)) -form- -parent- -environment-))
         (declare (ignorable -parent- -environment-)
                  ,@(function-like-definer-declarations -options-))
         ,(%walker-handler-body body))
       ',name)))

#+nil ; not good as it is...
(def (definer e :available-flags "od") walker-function (name args &body body)
  (with-standard-definer-options name
    (%walker-handler-function-body name args body (function-like-definer-declarations -options-))))

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

;;;
;;; Base AST form class
;;;

(def (generic e) copy-ast-slots (new old)
  (:documentation "Copies slots from old to new")
  (:method-combination progn :most-specific-last))

(def (generic e) map-ast (visitor form)
  (:documentation "Recursively descend main links of the tree.")
  (:method-combination progn :most-specific-last)
  (:method :around (visitor form)
    (let ((new (funcall visitor form)))
      ;; if the visitor returns a new AST node instead of the one
      ;; being given to it, then stop descending the tree and just
      ;; return the new one giving full control to the visitor over
      ;; what to do there.
      (if (eq new form)
          (call-next-method)
          new)
      new))
  (:method progn (visitor (form t))
    ;; a primary method with a huge NOP
    (declare (ignore visitor)))
  (:method progn (visitor (form cons))
    (map-ast visitor (car form))
    (map-ast visitor (cdr form))))

(def (generic e) rewrite-ast-fields (form visitor &key skip-main-refs include-back-refs)
  (:documentation "Rewrite tree links using the visitor.")
  (:method-combination progn :most-specific-last)
  (:method progn ((form t) visitor &key skip-main-refs include-back-refs)
    ;; a primary method with a huge NOP
    (declare (ignore form visitor skip-main-refs include-back-refs))))

(def function rewrite-tree (parent slot-name visitor value)
  "Apply visitor to all non-nil leaf values of a cons tree."
  (cond ((null value) nil)
        ((consp value)
         (mapcar (lambda (item)
                   (rewrite-tree parent slot-name visitor item))
                 value))
        (t
         (funcall visitor parent slot-name value))))

;; Form class definer. Also defines methods for the above functions.

(def (definer e) form-class (name supers slots &rest args)
  (let* ((new-supers (if (and (not (eq name 'walked-form))
                              (zerop (length supers)))
                         '(walked-form)
                         supers))
         (copy-forms nil)
         (main-refs nil)
         (back-refs nil)
         (flags (if (getf -options- :export t) '(ea) ()))
         (new-slots
          (mapcar (lambda (flags)
                    (let ((slot (pop flags)))
                      ;; Replicate logic from class*
                      (when (oddp (length flags))
                        (push :initform flags))
                      ;; Copy by default; allow conversion
                      (awhen (getf flags :copy-with t)
                        (push `(when (slot-boundp old ',slot)
                                 (setf (slot-value new ',slot)
                                       ,(if (eq it t)
                                            `(slot-value old ',slot)
                                            `(funcall ,it (slot-value old ',slot)))))
                              copy-forms))
                      ;; Main (parent->child) and back links:
                      (ecase (getf flags :ast-link)
                        ((t :main) (push slot main-refs))
                        ((:back)   (push slot back-refs))
                        ((nil))) ; Valid, but NOP
                      ;; Remove the non-standard attributes
                      (list* slot (remove-from-plist flags :ast-link :copy-with))))
                  (mapcar #'ensure-list slots)))
         (bodies
          (list `(def (class* ,@flags) ,name ,new-supers ,new-slots ,@args))))
    ;; Generate AST manipulation methods
    (when copy-forms
      (push `(defmethod copy-ast-slots progn ((new ,name) (old ,name))
               ,@(nreverse copy-forms))
            bodies))
    (when main-refs
      (setf main-refs (nreverse main-refs))
      (push `(defmethod map-ast progn (visitor (form ,name))
               ,@(loop
                    :for slot :in main-refs
                    :collect `(map-ast visitor (slot-value form ',slot))))
            bodies))
    (when (or main-refs back-refs)
      (setf back-refs (nreverse back-refs))
      (push `(defmethod rewrite-ast-fields progn ((form ,name) visitor &key
                                                  skip-main-refs include-back-refs)
               (declare (ignorable skip-main-refs include-back-refs))
               ,(if main-refs
                    `(unless skip-main-refs
                       ,@(loop
                            :for slot :in main-refs
                            :collect `(setf (slot-value form ',slot)
                                            (rewrite-tree form ',slot visitor
                                                          (slot-value form ',slot))))))
               ,(if back-refs
                    `(when include-back-refs
                       ,@(loop
                            :for slot :in back-refs
                            :collect `(setf (slot-value form ',slot)
                                            (rewrite-tree form ',slot visitor
                                                          (slot-value form ',slot)))))))
            bodies))
    `(progn ,@(nreverse bodies))))

;; Root form class

(def form-class walked-form ()
  ((parent)
   (source *current-form*)
   (attributes nil :copy-with #'copy-list)))

;; Form attributes

(def (macro e) form-attribute (form tag &optional default-value)
  "Access the attribute plist of a form."
  `(getf (attributes-of ,form) ,tag ,default-value))

(def (definer e :options "eod") form-attribute-accessor (key &key name type default (forms 'walked-form))
  (unless name
    (setf name (funcall *accessor-name-transformer*
                        key (if type (list :type type)))))
  `(progn
     ,@(if type
           `((declaim (ftype (function (walked-form) ,type) ,name)
                      (ftype (function (,type walked-form) ,type) (setf ,name)))))
     ,@(loop :for ftype :in (ensure-list forms)
          :collect `(def (method ,@-options-) ,name ((form ,ftype))
                      (form-attribute form ',key ,default)))
     ,@(loop :for ftype :in (ensure-list forms)
          :collect `(def (method ,@-options-) (setf ,name) (value (form ,ftype))
                      ,@(if (and type (getf -options- :debug))
                            `((check-type value ,type)))
                      (setf (form-attribute form ',key) value)))))

;; Named forms

(def form-class named-walked-form ()
  ((name)))

(def (function e) find-form-by-name (name forms &key (type 't))
  (check-type name symbol)
  (find-if (lambda (item)
             (and item
                  (or (eql type t)
                      (typep item type))
                  (eq (name-of item) name)))
           forms))

(def form-class name-definition-form (named-walked-form)
  ((usages :copy-with nil)))

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

(def (macro e) with-form-object ((variable type parent &rest initargs)
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
