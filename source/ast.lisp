;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.walker)

(def (generic e) map-ast (visitor form)
  (:documentation "Recursively descend main links of the tree.")
  (:method-combination progn)
  (:method :around (visitor form)
    (let ((new (funcall visitor form)))
      ;; if the visitor returns a new AST node instead of the one being given to it, then stop descending the tree and just return the new one
      ;; giving full control to the visitor over what to do there.
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
  (:method-combination progn)
  (:method progn ((form t) visitor &key skip-main-refs include-back-refs)
    ;; a primary method with a huge NOP
    (declare (ignore form visitor skip-main-refs include-back-refs))))

(def function rewrite-tree (parent accessor visitor value)
  "Apply visitor to all non-nil leaf values of a cons tree."
  (cond ((null value) nil)
        ((consp value)
         (mapcar (lambda (item)
                   (rewrite-tree parent accessor visitor item))
                 value))
        (t
         (funcall visitor parent accessor value))))

(def (macro e) define-walker-ast-fields (type &rest accessors)
  "Define AST-walking generic function methods for the type."
  (let ((has-main? (not (or (null accessors)
                            (eq (first accessors) :backrefs))))
        (backrefs  (cdr (member :backrefs accessors))))
    `(progn
       ;; Read-only recursive walking
       ,(if has-main?
            `(defmethod map-ast progn (visitor (form ,type))
               ,@(loop
                    :for accessor :in accessors
                    :until (eq accessor :backrefs)
                    :collect `(map-ast visitor (,accessor form)))))
       ;; Rewrite
       (defmethod rewrite-ast-fields progn ((form ,type) visitor &key
                                            skip-main-refs include-back-refs)
         (declare (ignorable skip-main-refs include-back-refs))
         ,(if has-main?
              `(unless skip-main-refs
                 ,@(loop
                      :for accessor :in accessors
                      :until (eq accessor :backrefs)
                      :collect `(setf (,accessor form)
                                      (rewrite-tree form ',accessor visitor
                                                    (,accessor form))))))
         ,(if backrefs
              `(when include-back-refs
                 ,@(loop
                      :for accessor :in backrefs
                      :collect `(setf (,accessor form)
                                      (rewrite-tree form ',accessor visitor
                                                    (,accessor form))))))))))

(macrolet ((frob (&rest entries)
             `(progn
                ,@(loop
                     :for (type . accessors) :in entries
                     :collect `(define-walker-ast-fields ,type ,@accessors)))))
  (frob
   (application-form                          operator-of arguments-of)
   (lambda-function-form                      arguments-of)
   (function-argument-form-with-default-value default-value-of)
   (implicit-progn-mixin                      body-of)
   (implicit-progn-with-declarations-mixin    declarations-of)
   (binder-form-mixin                         bindings-of)
   (lexical-variable-binding-form             initial-value-of)

   (lexical-application-form                  :backrefs definition-of)
   (walked-lexical-function-object-form       :backrefs definition-of)
   (walked-lexical-variable-reference-form    :backrefs definition-of)

   (return-from-form                          result-of :backrefs target-block-of)
   (throw-form                                value-of)
   (if-form                                   condition-of then-of else-of)
   (multiple-value-call-form                  arguments-of function-designator-of)
   (multiple-value-prog1-form                 first-form-of other-forms-of)
   (progv-form                                variables-form-of values-form-of)
   (setq-form                                 variable-of value-of)
   (go-form                                   :backrefs tag-of)
   (the-form                                  declared-type-of value-of)
   (unwind-protect-form                       protected-form-of cleanup-form-of)
   (load-time-value-form                      body-of)))

(def (function e) collect-variable-references (top-form &key (type 'variable-reference-form))
  (let ((result (list)))
    (map-ast (lambda (form)
               (when (typep form type)
                 (push form result))
               form)
             top-form)
    result))

(def function clear-binding-usage-annotation (top-form)
  (map-ast (lambda (form)
             (when (typep form 'name-definition-form)
               (setf (usages-of form) nil))
             form)
           top-form))

(def generic mark-binding-usage (form)
  (:method-combination progn)
  (:method progn ((form t)))
  (:method progn ((form walked-lexical-variable-reference-form))
    (push form (usages-of (definition-of form))))
  (:method progn ((form walked-lexical-function-object-form))
    (push form (usages-of (definition-of form))))
  (:method progn ((form walked-lexical-application-form))
    (push form (usages-of (definition-of form))))
  (:method progn ((form return-from-form))
    (push form (usages-of (target-block-of form))))
  (:method progn ((form go-form))
    (push form (usages-of (tag-of form)))))

(def (function e) annotate-binding-usage (top-form)
  (clear-binding-usage-annotation top-form)
  (map-ast (lambda (form)
             (mark-binding-usage form)
             form)
           top-form))

(def function copy-form (form &rest initargs)
  "Duplicate a form instance"
  ;; TODO: maybe define a generic function for this too?
  ;; If so, it should be implemented in the form-class
  ;; definer since it applies to absolutely all slots.
  (let* ((class (class-of form))
         (copy (allocate-instance class)))
      (dolist (slot (closer-mop:class-slots class))
        (let ((slot-name (closer-mop:slot-definition-name slot)))
          (when (slot-boundp form slot-name)
            (setf (slot-value copy slot-name)
                  (slot-value form slot-name)))))
      (apply #'reinitialize-instance copy initargs)))

(def (function e) rewrite-ast (top-form visitor &key parent parent-field)
  (labels ((rewrite-rec (parent field form)
             (multiple-value-bind (new-form continue)
                 (funcall visitor parent field form)
               (prog1 new-form
                 (when (or (eq form new-form) continue)
                   (rewrite-ast-fields new-form #'rewrite-rec))))))
    (rewrite-rec parent parent-field top-form)))

(def (function e) deep-copy-ast (top-form &key replace-cb parent parent-field
                                          (lookup-table (make-hash-table :test #'eq))
                                          upper-table)
  "Copy a form tree with proper distinction between back and forward refs."
  (labels ( ;; Used to fix back references
           (lookup-cb (parent field form)
             (declare (ignore parent field))
             (or (gethash form lookup-table) form))
           ;; Walk the main links, copying everything
           (clone-rec (parent field form)
             (if (typep form 'walked-form)
                 (let ((user-value (if replace-cb
                                       (funcall replace-cb parent field form)
                                       form)))
                   (if (not (eq user-value form))
                       user-value
                       (or (gethash form lookup-table)
                           ;; Actually copy the node
                           (let ((new-form (copy-instance form)))
                             (setf (parent-of new-form) parent)
                             (setf (gethash form lookup-table) new-form)
                             (rewrite-ast-fields new-form #'clone-rec)
                             new-form))))
                 form)))
    (prog1
        (clone-rec parent parent-field top-form)
      ;; Rewrite backrefs to copied forms
      (maphash (lambda (key new-form)
                 (declare (ignore key))
                 (when upper-table
                   (setf (gethash new-form upper-table) new-form))
                 (rewrite-ast-fields new-form #'lookup-cb
                                     :skip-main-refs t
                                     :include-back-refs t))
               lookup-table))))

(def (function e) substitute-ast-if (new test tree &key in-place first-without-copy)
  (let ((must-copy? (not first-without-copy))
        (up-table (if in-place nil (make-hash-table :test #'eq))))
    (flet ((handle-node (parent field form)
             (declare (ignore field))
             (if (funcall test form)
                 (let ((new-form (if must-copy?
                                     (deep-copy-ast new :upper-table up-table)
                                     new)))
                   (setf (parent-of new-form) parent)
                   (setf must-copy? t)
                   new-form)
                 form)))
      (if in-place
          (rewrite-ast tree #'handle-node)
          (deep-copy-ast tree :replace-cb #'handle-node :lookup-table up-table)))))

