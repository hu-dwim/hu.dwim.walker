;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See COPYING for details.

(in-package :cl-walker)

;;;; Atoms

(defclass constant-form (form)
  ((value :accessor value-of :initarg :value)))

(defunwalker-handler constant-form (value)
  (if (or (eq value t)
          (eq value nil))
      value
      (typecase value
        (symbol `(quote ,value))
        (cons   `(quote ,value))
        (t value))))

(defclass variable-reference-form (form)
  ((name :accessor name-of :initarg :name)))

(defunwalker-handler variable-reference-form (name)
  name)

(defmethod print-object ((v variable-reference-form) stream)
  (print-unreadable-object (v stream :type t :identity t)
    (format stream "~S" (name-of v))))

(defclass lexical-variable-reference-form (variable-reference-form)
  ())

(defclass walked-lexical-variable-reference-form (lexical-variable-reference-form)
  ()
  (:documentation "A reference to a local variable defined in the lexical environment inside the form passed to walk-form."))

(defclass unwalked-lexical-variable-reference-form (lexical-variable-reference-form)
  ()
  (:documentation "A reference to a local variable defined in the lexical environment outside of the form passed to walk-form."))

(defclass free-variable-reference-form (variable-reference-form)
  ())

(defun special-variable-name? (name)
  (or (boundp name)
      #+sbcl(eq (sb-int:info :variable :kind name) :special)))

(defwalker-handler +atom-marker+ (form parent env)
  (cond
    ((or (eq form t)
         (eq form nil)
         (not (or (symbolp form)
                  (consp form))))
     (make-instance 'constant-form :value form
                    :parent parent :source form))
    ((lookup-in-walkenv :variable form env)
     (make-instance 'walked-lexical-variable-reference-form :name form
                    :parent parent :source form))
    ((lookup-in-walkenv :unwalked-variable form env)
     (make-instance 'unwalked-lexical-variable-reference-form :name form
                    :parent parent :source form))
    ((lookup-in-walkenv :symbol-macro form env)
     (walk-form (lookup-in-walkenv :symbol-macro form env) parent env))
    ((nth-value 1 (macroexpand-1 form))
     ;; a globaly defined symbol-macro
     (walk-form (macroexpand-1 form) parent env))
    (t
     (when (and *warn-undefined*
                (not (special-variable-name? form)))
       (warn 'undefined-variable-reference :name form))
     (make-instance 'free-variable-reference-form :name form
                    :parent parent :source form))))

;;;; BLOCK/RETURN-FROM

(defclass block-form (form implicit-progn-mixin)
  ((name :accessor name-of :initarg :name)))

(defwalker-handler block (form parent env)
  (destructuring-bind (block-name &rest body)
      (cdr form)
    (with-form-object (block block-form
                       :parent parent :source form
                       :name block-name)
      (setf (body-of block) (walk-implict-progn block
                                                body
                                                (augment-walkenv env :block block-name block))))))

(defunwalker-handler block-form (name body)
  `(block ,name ,@(unwalk-forms body)))

(defclass return-from-form (form)
  ((target-block :accessor target-block-of :initarg :target-block)
   (result :accessor result-of :initarg :result)))


(define-condition return-from-unknown-block (error)
  ((block-name :accessor block-name :initarg :block-name))
  (:report (lambda (condition stream)
             (format stream "Unable to return from block named ~S." (block-name condition)))))

(defwalker-handler return-from (form parent env)
  (destructuring-bind (block-name &optional (value '(values)))
      (cdr form)
    (if (lookup-in-walkenv :block block-name env)
        (with-form-object (return-from return-from-form :parent parent :source form
                           :target-block (lookup-in-walkenv :block block-name env))
          (setf (result-of return-from) (walk-form value return-from env)))
        (restart-case
            (error 'return-from-unknown-block :block-name block-name)
          (add-block ()
            :report "Add this block and continue."
            (walk-form form parent (augment-walkenv env :block block-name :unknown-block)))))))

(defunwalker-handler return-from-form (target-block result)
  `(return-from ,(name-of target-block) ,(unwalk-form result)))

;;;; CATCH/THROW

(defclass catch-form (form implicit-progn-mixin)
  ((tag :accessor tag-of :initarg :tag)))

(defwalker-handler catch (form parent env)
  (destructuring-bind (tag &body body)
      (cdr form)
    (with-form-object (catch catch-form :parent parent :source form)
      (setf (tag-of catch) (walk-form tag catch env)
            (body-of catch) (walk-implict-progn catch body env)))))

(defunwalker-handler catch-form (tag body)
  `(catch ,(unwalk-form tag) ,@(unwalk-forms body)))

(defclass throw-form (form)
  ((tag :accessor tag-of :initarg :tag)
   (value :accessor value-of :initarg :value)))

(defwalker-handler throw (form parent env)
  (destructuring-bind (tag &optional (result '(values)))
      (cdr form)
    (with-form-object (throw throw-form :parent parent :source form)
      (setf (tag-of throw) (walk-form tag throw env)
            (value-of throw) (walk-form result throw env)))))

(defunwalker-handler throw-form (tag value)
  `(throw ,(unwalk-form tag) ,(unwalk-form value)))

;;;; EVAL-WHEN

(defclass eval-when-form (form implicit-progn-mixin)
  ((eval-when-times :accessor eval-when-times :initarg :eval-when-times)))

(defwalker-handler eval-when (form parent env)
  (destructuring-bind (times &body body)
      (cdr form)
    (with-form-object (eval-when eval-when-form :parent parent :source form)
      (setf (eval-when-times eval-when) times
            (body-of eval-when) (walk-implict-progn eval-when body env)))))

(defunwalker-handler eval-when-form (body eval-when-times)
  `(eval-when ,eval-when-times
     ,@(unwalk-forms body)))

;;;; IF

(defclass if-form (form)
  ((condition :accessor condition-of :initarg :condition)
   (then :accessor then-of :initarg :then)
   (else :accessor else-of :initarg :else)))

(defwalker-handler if (form parent env)
  (with-form-object (if if-form :parent parent :source form)
    (setf (condition-of if) (walk-form (second form) if env)
          (then-of if) (walk-form (third form) if env)
          (else-of if) (walk-form (fourth form) if env))))

(defunwalker-handler if-form (condition then else)
  `(if ,(unwalk-form condition) ,(unwalk-form then) ,(unwalk-form else)))

;;;; LET/LET*

(defclass variable-binding-form (form binding-form-mixin implicit-progn-with-declare-mixin)
  ())

(defclass let-form (variable-binding-form)
  ())

(defwalker-handler let (form parent env)
  (with-form-object (let let-form :parent parent :source form)
    (setf (bindings-of let) (mapcar (lambda (binding)
                                      (destructuring-bind (var &optional initial-value)
                                          (ensure-list binding)
                                        (cons var (walk-form initial-value let env))))
                                    (second form)))
    (multiple-value-bind (b e d declarations)
        (split-body (cddr form) env :parent let :declare t)
      (declare (ignore b e d))
      (loop
         :for (var . value) :in (bindings-of let)
         :do (unless (find-if (lambda (declaration)
                                (and (typep declaration 'special-variable-declaration-form)
                                     (eq var (name-of declaration))))
                              declarations)
               ;; TODO audit this part, :dummy? check other occurrances, too!
               (augment-walkenv! env :variable var :dummy)))
      (multiple-value-setf ((body-of let) _ (declares let))
                           (walk-implict-progn let (cddr form) env :declare t)))))

(defunwalker-handler let-form (bindings body declares)
  `(let ,(mapcar (lambda (bind)
                   (list (car bind) (unwalk-form (cdr bind))))
                 bindings)
     ,@(unwalk-declarations declares)
     ,@(unwalk-forms body)))

(defclass let*-form (variable-binding-form)
  ())

(defwalker-handler let* (form parent env)
  (with-form-object (let* let*-form :parent parent :source form :bindings '())
    (dolist* ((var &optional initial-value) (mapcar #'ensure-list (second form)))
      (push (cons var (walk-form initial-value let* env)) (bindings-of let*))
      (augment-walkenv! env :variable var :dummy))
    (setf (bindings-of let*) (nreverse (bindings-of let*)))
    (multiple-value-setf ((body-of let*) _ (declares let*))
      (walk-implict-progn let* (cddr form) env :declare t))))

(defunwalker-handler let*-form (bindings body declares)
  `(let* ,(mapcar (lambda (bind)
                    (list (car bind) (unwalk-form (cdr bind))))
                  bindings)
     ,@(unwalk-declarations declares)
     ,@(unwalk-forms body)))

;;;; LOAD-TIME-VALUE

(defclass load-time-value-form (form)
  ((value :accessor value-of)
   (read-only-p :accessor read-only-p)))

(defwalker-handler load-time-value (form parent env)
  (with-form-object (load-time-value load-time-value-form
                                     :parent parent :source form)
    (setf (value-of load-time-value) (walk-form (second form) load-time-value env)
          (read-only-p load-time-value) (third form))))

(defunwalker-handler load-time-value-form (body read-only)
  `(load-time-value ,(unwalk-form body) ,read-only))

;;;; LOCALLY

(defclass locally-form (form implicit-progn-with-declare-mixin)
  ())

(defwalker-handler locally (form parent env)
  (with-form-object (locally locally-form :parent parent :source form)
    (multiple-value-setf ((body-of locally) _ (declares locally))
      (walk-implict-progn locally (cdr form) env :declare t))))

(defunwalker-handler locally-form (body declares)
  `(locally ,@(unwalk-declarations declares)
     ,@(unwalk-forms body)))

;;;; MACROLET

(defclass macrolet-form (form binding-form-mixin implicit-progn-with-declare-mixin)
  ())

(defwalker-handler macrolet (form parent env)
  (with-form-object (macrolet macrolet-form :parent parent :source form
                              :bindings '())
    (dolist* ((name args &body body) (second form))
      (let ((handler (parse-macro-definition name args body (cdr env))))
        (augment-walkenv! env :macro name handler)
        (push (cons name handler) (bindings-of macrolet))))
    (setf (bindings-of macrolet) (nreverse (bindings-of macrolet)))
    (multiple-value-setf ((body-of macrolet) _ (declares macrolet))
      (walk-implict-progn macrolet (cddr form) env :declare t))))

(defunwalker-handler macrolet-form (body bindings declares)
  ;; We ignore the bindings, because the expansion has already taken
  ;; place at walk-time.
  (declare (ignore bindings))
  `(locally ,@(unwalk-declarations declares) ,@(unwalk-forms body)))

;;;; MULTIPLE-VALUE-CALL

(defclass multiple-value-call-form (form)
  ((function-designator :accessor function-designator-of :initarg :function-designator)
   (arguments :accessor arguments-of :initarg :arguments)))

(defwalker-handler multiple-value-call (form parent env)
  (with-form-object (m-v-c multiple-value-call-form :parent parent :source form)
    (setf (function-designator-of m-v-c) (walk-form (second form) m-v-c env)
          (arguments-of m-v-c) (mapcar (lambda (f) (walk-form f m-v-c env))
                                       (cddr form)))))

(defunwalker-handler multiple-value-call-form (function-designator arguments)
  `(multiple-value-call ,(unwalk-form function-designator) ,@(unwalk-forms arguments)))

;;;; MULTIPLE-VALUE-PROG1

(defclass multiple-value-prog1-form (form)
  ((first-form :accessor first-form-of :initarg :first-form)
   (other-forms :accessor other-forms-of :initarg :other-forms)))

(defwalker-handler multiple-value-prog1 (form parent env)
  (with-form-object (m-v-p1 multiple-value-prog1-form :parent parent :source form)
    (setf (first-form-of m-v-p1) (walk-form (second form) m-v-p1 env)
          (other-forms-of m-v-p1) (mapcar (lambda (f) (walk-form f m-v-p1 env))
                                       (cddr form)))))

(defunwalker-handler multiple-value-prog1-form (first-form other-forms)
  `(multiple-value-prog1 ,(unwalk-form first-form) ,@(unwalk-forms other-forms)))

;;;; PROGN

(defclass progn-form (form implicit-progn-mixin)
  ())

(defwalker-handler progn (form parent env)
  (with-form-object (progn progn-form :parent parent :source form)
    (setf (body-of progn) (walk-implict-progn progn (cdr form) env))))

(defunwalker-handler progn-form (body)
  `(progn ,@(unwalk-forms body)))

;;;; PROGV

(defclass progv-form (form implicit-progn-mixin)
  ((variables-form :accessor variables-form-of :initarg :variables-form)
   (values-form :accessor values-form-of :initarg :values-form)))

(defwalker-handler progv (form parent env)
  (with-form-object (progv progv-form :parent parent :source form)
    (setf (variables-form-of progv) (walk-form (cadr form) progv env))
    (setf (values-form-of progv) (walk-form (caddr form) progv env))
    (setf (body-of progv) (walk-implict-progn progv (cdddr form) env))
    progv))

(defunwalker-handler progv-form (body variables-form values-form)
  `(progv ,(unwalk-form variables-form) ,(unwalk-form values-form) ,@(unwalk-forms body)))

;;;; QUOTE

(defwalker-handler quote (form parent env)
  (make-instance 'constant-form :parent parent :source form :value (second form)))

;;;; SETQ

(defclass setq-form (form)
  ((variable-name
    :accessor variable-name-of
    :initarg :variable-name)
   (value
    :accessor value-of
    :initarg :value)))

(defwalker-handler setq (form parent env)
  ;; the SETQ handler needs to be able to deal with symbol-macrolets
  ;; which haven't yet been expanded and may expand into something
  ;; requiring setf and not setq.
  (let ((effective-code '()))
    (loop
       :for (name value) :on (cdr form) :by #'cddr
       :for symbol-macro = (lookup-in-walkenv :symbol-macro name env)
       :if symbol-macro
         :do (push `(setf ,symbol-macro ,value) effective-code)
       :else
         :do (push `(setq ,name ,value) effective-code))
    (if (= 1 (length effective-code))
        ;; only one form, the "simple case"
        (destructuring-bind (type var value)
            (first effective-code)
          (ecase type
            (setq (with-form-object (setq setq-form :parent parent :source form
                                          :variable-name var)
                    (setf (value-of setq) (walk-form value setq env))))
            (setf (walk-form (first effective-code) parent env))))
        ;; multiple forms
        (with-form-object (progn progn-form :parent parent :source form)
          (setf (body-of progn) (walk-implict-progn progn effective-code env))))))

(defunwalker-handler setq-form (variable-name value)
  `(setq ,variable-name ,(unwalk-form value)))

;;;; SYMBOL-MACROLET

(defclass symbol-macrolet-form (form binding-form-mixin implicit-progn-with-declare-mixin)
  ())

(defwalker-handler symbol-macrolet (form parent env)
  (with-form-object (symbol-macrolet symbol-macrolet-form :parent parent :source form
                                     :bindings '())
    (dolist* ((symbol expansion) (second form))
      (augment-walkenv! env :symbol-macro symbol expansion)
      (push (cons symbol expansion) (bindings-of symbol-macrolet)))
    (setf (bindings-of symbol-macrolet) (nreverse (bindings-of symbol-macrolet)))
    (multiple-value-setf ((body-of symbol-macrolet) _ (declares symbol-macrolet))
      (walk-implict-progn symbol-macrolet (cddr form) env :declare t))))

(defunwalker-handler symbol-macrolet-form (body bindings declares)
  ;; We ignore the bindings, because the expansion has already taken
  ;; place at walk-time.
  (declare (ignore bindings))
  `(locally ,@(unwalk-declarations declares) ,@(unwalk-forms body)))

;;;; TAGBODY/GO

(defclass tagbody-form (form implicit-progn-mixin)
  ())

(defwalker-handler tagbody (form parent env)
  (with-form-object (tagbody tagbody-form :parent parent :source form :body (cdr form))
    (augment-walkenv! env :tagbody 'enclosing-tagbody tagbody)
    (flet ((go-tag-p (form)
             (or (symbolp form) (integerp form))))
      ;; the loop below destructuivly modifies the body of tagbody,
      ;; since it's the same object as the source we need to copy it.
      (setf (body-of tagbody) (copy-list (body-of tagbody)))
      (loop
         :for part :on (body-of tagbody)
         :if (go-tag-p (car part))
           :do (augment-walkenv! env :tag (car part) (cdr part)))
      (loop
         :for part :on (body-of tagbody)
         :if (go-tag-p (car part))
           :do (setf (car part) (make-instance 'go-tag-form :parent tagbody
                                               :source (car part)
                                               :name (car part)))
         :else
           :do (setf (car part) (walk-form (car part) tagbody env))))))

(defunwalker-handler tagbody-form (body)
  `(tagbody ,@(unwalk-forms body)))

(defclass go-tag-form (form)
  ((name :accessor name-of :initarg :name)))

(defunwalker-handler go-tag-form (name)
  name)

(defclass go-form (form)
  ((jump-target :accessor jump-target-of :initarg :jump-target)
   (name :accessor name-of :initarg :name)
   (enclosing-tagbody :accessor enclosing-tagbody-of :initarg :enclosing-tagbody)))

(defwalker-handler go (form parent env)
  (make-instance 'go-form
                 :parent parent
                 :source form
                 :name (second form)
                 :jump-target (lookup-in-walkenv :tag (second form) env)
                 :enclosing-tagbody (lookup-in-walkenv :tagbody 'enclosing-tagbody env)))

(defunwalker-handler go-form (name)
  `(go ,name))

;;;; THE

(defclass the-form (form)
  ((type :accessor type-of :initarg :type)
   (value :accessor value-of :initarg :value)))

(defwalker-handler the (form parent env)
  (with-form-object (the the-form :parent parent :source form
                                  :type (second form))
    (setf (value-of the) (walk-form (third form) the env))))

(defunwalker-handler the-form (type value)
  `(the ,type ,(unwalk-form value)))

;;;; UNWIND-PROTECT

(defclass unwind-protect-form (form)
  ((protected-form :accessor protected-form-of :initarg :protected-form)
   (cleanup-form :accessor cleanup-form-of :initarg :cleanup-form)))

(defwalker-handler unwind-protect (form parent env)
  (with-form-object (unwind-protect unwind-protect-form :parent parent
                                    :source form)
    (setf (protected-form-of unwind-protect) (walk-form (second form) unwind-protect env)
          (cleanup-form-of unwind-protect) (walk-implict-progn unwind-protect (cddr form) env))))

(defunwalker-handler unwind-protect-form (protected-form cleanup-form)
  `(unwind-protect ,(unwalk-form protected-form) ,@(unwalk-forms cleanup-form)))

;;;; LOAD-TIME-VALUE

(defclass load-time-value-form (form)
  ((body :accessor body-of :initarg :body)
   (read-only :initform nil :accessor read-only-p :initarg :read-only)
   (value :accessor value-of)))

(defmethod initialize-instance :after ((self load-time-value-form) &key)
  (setf (value-of self) (eval (body-of self))))

(defwalker-handler load-time-value (form parent env)
  (assert (<= (length form) 3))
  (with-form-object (load-time-value load-time-value-form :parent parent
                                     :body form
                                     :read-only (third form))
    (setf (body-of load-time-value) (walk-form (second form)))))
