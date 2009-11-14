;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.walker)

;;;
;;; Clozure CL
;;;

(defun make-empty-lexical-environment ()
  (ccl::new-lexical-environment))

;;;
;;; utilities
;;;

(defun ccl-defenv-p (env)
  (ccl::istruct-typep env 'ccl::definition-environment))

(defmacro do-ccl-env-chain ((env-var env-item &key with-defenv) &body code)
  `(do ((,env-var ,env-item (ccl::lexenv.parent-env ,env-var)))
       ((or (null ,env-var)
            ,(if with-defenv
                 `(consp ,env-var)
                 `(ccl-defenv-p ,env-var))))
     ,@code))

(defun ccl-get-var-decls (var-name env)
  (loop
     for dec in (ccl::lexenv.vdecls env)
     when (eq (car dec) var-name)
     collect (cdr dec)))

(defun ccl-get-env-vars (env)
  ;; The variable list field may contain a special
  ;; barrier sentinel. Ignore it.
  (let ((lst (ccl::lexenv.variables env)))
    (if (listp lst) lst)))

(defun ccl-proclaimed-special-p (name &optional lexenv)
  ;; During compilation the special proclamations are
  ;; collected in the definition environment.
  (let* ((defenv (ccl::definition-environment lexenv))
         (specials (if defenv (ccl::defenv.specials defenv))))
    (or (ccl::assq name specials)
        (ccl:proclaimed-special-p name))))

(defun ccl-defined-const-p (name &optional lexenv)
  (let* ((defenv (ccl::definition-environment lexenv))
         (consts (if defenv (ccl::defenv.constants defenv))))
    (ccl::assq name consts)))

(defun ccl-ignored-decl-p (decls)
  (cdr (ccl::assq 'ignore decls)))

(defun ccl-special-decl-p (decls)
  ;; Presence alone means it's special
  (ccl::assq 'special decls))

(defun ccl-symbol-macro-p (var-spec)
  (let ((exp (ccl::var-expansion var-spec)))
    (and (consp exp)
         (eql :symbol-macro (car exp)))))

;;;
;;; iteration
;;;

(defun iterate-variables-in-lexenv (visitor lexenv
                                    &key include-ignored? include-specials? include-macros?
                                    &aux hide-list)
  (do-ccl-env-chain (env lexenv :with-defenv t)
    ;; Local functions spawn temporaries; hide them
    (dolist (func-spec (ccl::lexenv.functions env))
      (when (eql 'ccl::function (cadr func-spec))
        (push (cdddr func-spec) hide-list)))
    (if (ccl-defenv-p env)
        (progn
          (when include-macros?
            (dolist (cell (ccl::defenv.symbol-macros env))
              (funcall visitor (car cell) :macro? t :macro-body (cdr cell)))))
        ;; Enumerate vars
        (let* ((decls         (ccl::lexenv.vdecls env))
               (special-decls (remove 'special decls :key #'cadr :test-not #'eq)))
          (dolist (var-spec (ccl-get-env-vars env))
            (let* ((name      (ccl::var-name var-spec))
                   (macro?    (ccl-symbol-macro-p var-spec))
                   (ignored?  (find-if (lambda (item)
                                         (and (eq (first item) name)
                                              (eq (second item) 'ignore)
                                              (cddr item)))
                                       decls))
                   (special?  (find name special-decls :key #'car)))
              (when special?
                (deletef special-decls name :key #'car))
              (if macro?
                  (when include-macros?
                    (funcall visitor name :macro? t
                             :macro-body (cdr (ccl::var-expansion var-spec))))
                  (when (and (or (not ignored?)
                                 include-ignored?)
                             (or (not special?)
                                 include-specials?)
                             (not (member name hide-list)))
                    (funcall visitor name :ignored? ignored? :special? special?)))))
          ;; Enumerate var-less special decls as vars
          (when include-specials?
            (dolist (decl special-decls)
              (funcall visitor (car decl) :special? t)))))))

(defun iterate-functions-in-lexenv (visitor lexenv &key include-macros?)
  (do-ccl-env-chain (env lexenv :with-defenv t)
    ;; lexenv.functions can operate on a defenv
    (dolist (func-spec (ccl::lexenv.functions env))
      (let* ((name      (ccl::maybe-setf-name (first func-spec)))
             (function? (eql 'ccl::function (second func-spec)))
             (macro?   (eql 'ccl::macro (second func-spec))))
        (when (and macro? include-macros?)
          (assert (functionp (cddr func-spec)))
          (funcall visitor name :macro? t :macro-function (cddr func-spec)))
        (when function?
          (funcall visitor name))))))

(defun iterate-blocks-in-lexenv (visitor lexenv)
  (declare (ignore visitor lexenv))
  (cerror "ignore and do nothing"
          "The lexical environment does not contain blocks in Clozure CL"))

(defun iterate-tags-in-lexenv (visitor lexenv)
  (declare (ignore visitor lexenv))
  (cerror "ignore and do nothing"
          "The lexical environment does not contain tags in Clozure CL"))

;;;
;;; augmentation
;;;

(defun augment-lexenv-with-variable (name lexenv &key special ignored)
  (let* ((decls (if special `((special ,name))))
         (env (ccl:augment-environment lexenv :variable (list name) :declare decls)))
    ;; augment-environment does not understand ignore decls
    (when ignored
      (push (list* name 'ignore t) (ccl::lexenv.vdecls env)))
    env))

(defun augment-lexenv-with-function (name lexenv)
  (ccl:augment-environment lexenv :function (list name)))

(defun augment-lexenv-with-macro (name def lexenv)
  (ccl:augment-environment lexenv :macro (list (list name def))))

(defun augment-lexenv-with-symbol-macro (name def lexenv)
  (ccl:augment-environment lexenv :symbol-macro (list (list name def))))

(defun augment-lexenv-with-block (name lexenv)
  (declare (ignore name))
  ;; Do nothing
  lexenv)

(defun augment-lexenv-with-tag (name lexenv)
  (declare (ignore name))
  ;; Do nothing
  lexenv)

