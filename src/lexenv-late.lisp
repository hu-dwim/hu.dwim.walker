;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See COPYING for details.

(in-package :cl-walker)

(defparameter *lexical-environment-functions*
  '((make-empty-lexenv "Returns an empty lexical environment useful for testing and playing around in the repl.")
    (iterate-variables-in-lexenv     "(funcall VISITOR name ignored?) for each variable definition in LEXENV.")
    (iterate-functions-in-lexenv     "(funcall VISITOR name) for each function definition in LEXENV.")
    (iterate-macros-in-lexenv        "(funcall VISITOR name macro-function) for each macro definition in LEXENV.")
    (iterate-symbol-macros-in-lexenv "(funcall VISITOR name macro-function) for each symbol macro definition in LEXENV.")
    (augment-lexenv-with-variable)
    (augment-lexenv-with-function)
    (augment-lexenv-with-macro)
    (augment-lexenv-with-symbol-macro)))

;;; set up some docstrings
(loop
   :for (name documentation) :in *lexical-environment-functions*
   :do (when documentation
         (setf (documentation name 'function) documentation)))

(defun unimplemented-lexical-environment-function ()
  (cerror "ignore and try to continue" "This is not implemented for your lisp, sorry. You may try to continue, but...")
  nil)

;;; if there was no definition provided for some of the functions in
;;; *lexical-environment-functions* then register a function that will
;;; signal an error.
(eval-when (:load-toplevel :execute)
  (loop
     :for (name) :in *lexical-environment-functions*
     :do (unless (fboundp name)
           (setf (fdefinition name)
                 (lambda (&rest args)
                   (declare (ignore args))
                   (unimplemented-lexical-environment-function))))))

;;;
;;; variables
;;;
(defmacro do-variables-in-lexenv ((lexenv name &optional (ignored? (gensym) ignored-provided?))
                                  &body body)
  `(iterate-variables-in-lexenv
    (lambda (,name ,ignored?)
      ,@(unless ignored-provided?
        `((declare (ignore ,ignored?))))
      ,@body)
    ,lexenv
    :include-ignored ,ignored-provided?))

(defun collect-variables-in-lexenv (lexenv &key include-ignored filter)
  (let ((result (list)))
    (iterate-variables-in-lexenv
     (lambda (name ignored?)
       (when (or (not filter)
                 (funcall filter name ignored?))
         (push name result)))
     lexenv
     :include-ignored include-ignored)
    (nreverse result)))

(defun find-variable-in-lexenv (name-to-find lexenv &key include-ignored)
  (iterate-variables-in-lexenv
   (lambda (name ignored?)
     (when (eq name name-to-find)
       (return-from find-variable-in-lexenv (values name ignored?))))
   lexenv
   :include-ignored include-ignored)
  (values nil))

;;;
;;; functions
;;;
(defmacro do-functions-in-lexenv ((lexenv name) &body body)
  `(iterate-functions-in-lexenv
    (lambda (,name)
      ,@body)
    ,lexenv))

(defun collect-functions-in-lexenv (lexenv &key filter)
  (let ((result (list)))
    (iterate-functions-in-lexenv
     (lambda (name)
       (when (or (not filter)
                 (funcall filter name))
         (push name result)))
     lexenv)
    (nreverse result)))

(defun find-function-in-lexenv (name-to-find lexenv)
  (iterate-functions-in-lexenv
   (lambda (name)
     (when (eq name name-to-find)
       (return-from find-function-in-lexenv (values name))))
   lexenv)
  (values nil))

;;;
;;; macros
;;;
(defmacro do-macros-in-lexenv ((lexenv name &optional (macro-fn (gensym) macro-fn-provided?))
                               &body body)
  `(iterate-macros-in-lexenv
    (lambda (,name ,macro-fn)
      ,@(unless macro-fn-provided?
        `((declare (ignore ,macro-fn))))
      ,@body)
    ,lexenv))

(defun collect-macros-in-lexenv (lexenv &key filter)
  (let ((result (list)))
    (iterate-macros-in-lexenv
     (lambda (name macro-function)
       (declare (ignore macro-function))
       (when (or (not filter)
                 (funcall filter name))
         (push name result)))
     lexenv)
    (nreverse result)))

(defun find-macro-in-lexenv (name-to-find lexenv)
  (iterate-macros-in-lexenv
   (lambda (name macro-function)
     (when (eq name name-to-find)
       (return-from find-macro-in-lexenv (values name macro-function))))
   lexenv)
  (values nil))

;;;
;;; symbol-macros
;;;
(defmacro do-symbol-macros-in-lexenv ((lexenv name &optional (definition (gensym) definition-provided?))
                                      &body body)
  `(iterate-symbol-macros-in-lexenv
    (lambda (,name ,definition)
      ,@(unless definition-provided?
        `((declare (ignore ,definition))))
      ,@body)
    ,lexenv))

(defun collect-symbol-macros-in-lexenv (lexenv &key filter)
  (let ((result (list)))
    (iterate-symbol-macros-in-lexenv
     (lambda (name macro-body)
       (declare (ignore macro-body))
       (when (or (not filter)
                 (funcall filter name))
         (push name result)))
     lexenv)
    (nreverse result)))

(defun find-symbol-macro-in-lexenv (name-to-find lexenv)
  (iterate-symbol-macros-in-lexenv
   (lambda (name macro-body)
     (when (eq name name-to-find)
       (return-from find-symbol-macro-in-lexenv (values name macro-body))))
   lexenv)
  (values nil))

;;;
;;; blocks
;;;
(defmacro do-blocks-in-lexenv ((lexenv name) &body body)
  `(iterate-blocks-in-lexenv
    (lambda (,name)
      ,@body)
    ,lexenv))

(defun collect-blocks-in-lexenv (lexenv &key filter)
  (let ((result (list)))
    (iterate-blocks-in-lexenv
     (lambda (name)
       (when (or (not filter)
                 (funcall filter name))
         (push name result)))
     lexenv)
    (nreverse result)))

(defun find-block-in-lexenv (name-to-find lexenv)
  (iterate-blocks-in-lexenv
   (lambda (name)
     (when (eq name name-to-find)
       (return-from find-block-in-lexenv (values name))))
   lexenv)
  (values nil))

#||

TODO move these to their own standalone file and implement as the new api. see lexenv-sbcl.lisp for an example.

;;;
;;; OpenMCL
;;;
#+openmcl
(progn

(defmethod lexical-variables ((environment ccl::lexical-environment))
  (loop
     for env = environment
          then (ccl::lexenv.parent-env env)
     while (and env
                (not (ccl::istruct-typep env 'ccl::definition-environment)))
     for vars = (ccl::lexenv.variables env)
     when (listp vars)
     ;; we now weed out all symbol-macros and ignored variables
     append (remove-if (lambda (var-name)
                         (let ((decs (assoc var-name (ccl::lexenv.vdecls env))))
                           (and decs
                                (eql 'cl:ignore (second decs))
                                (eql 'cl:t (cddr decs)))))
                       (mapcar (lambda (var)
                                 ;; ccl::var-name is a macro, se we can't do #'ccl::var-name directly
                                 (ccl::var-name var))
                               (remove-if (lambda (var-spec)
                                            (and (ccl::var-ea var-spec)
                                                 (consp (ccl::var-ea var-spec))
                                                 (eql :symbol-macro (car (ccl::var-ea var-spec)))))
                                          vars)))))

(defmethod lexical-functions ((environment ccl::lexical-environment))
  (loop
     for env = environment
          then (ccl::lexenv.parent-env env)
     while (and env
                (not (ccl::istruct-typep env 'ccl::definition-environment)))
     for funs = (ccl::lexenv.functions env)
     when (listp funs)
     ;; we now weed out all symbol-macros and ignored variables
     append (mapcar (lambda (func-spec)
                      ;; convert the function name to a "real" function name
                      (let ((name (first func-spec)))
                        (if (eql (symbol-package (first func-spec))
                                 (find-package :SETF))
                            (list 'cl:setf (read-from-string (symbol-name name)))
                            name)))
                    (remove-if (lambda (func-spec)
                                 ;; weed out all the macrolets
                                 (eql 'ccl::macro (second func-spec)))
                               funs))))
) ; #+openmcl

;;;; ** CMUCL

#+cmu
(progn

(defmethod lexical-variables ((environment c::lexenv))
  (loop
     for var-spec in (c::lexenv-variables environment)
     ;; variable refs are (NAME . LAMBDA-VAR), we want to void
     ;; symbol-macrolets which are (NAME SYSTEM:MACRO . EXPANSION)
     when (and (atom (cdr var-spec))
               ;; don't return ignored vars
               (not (eq (type-of (cdr var-spec)) 'c::global-var))
               (not (c::lambda-var-ignorep (cdr var-spec))))
     collect (car var-spec)))

(defmethod lexical-functions ((environment c::lexenv))
  (loop
     for func-spec in (c::lexenv-functions environment)
     ;; flet and labels function look like ((FLET ACTUAL-NAME) . STUFF)
     if (and (consp (first func-spec))
             (member (car (first func-spec)) '(flet labels)))
       collect (second (first func-spec))
     ;; macrolets look like (NAME SYSTEM:MACRO . STUFF)
     else if (and (consp (cdr func-spec))
                  (eql 'system:macro (second func-spec)))
     ;; except that we don't return macros for now
     do (progn)
     ;; handle the case  (NAME . #<C::FUNCTIONAL>)
     else if (typep (cdr func-spec) 'C::FUNCTIONAL)
       collect (car func-spec)
     ;; if we get here we're confused :(
     else
       do (error "Sorry, don't know how to handle the lexcial function spec ~S."
                 func-spec)))

(defmethod lexical-macros ((environment c::lexenv))
  (loop
   for mac-spec in (c::lexenv-functions environment)
   when (and (consp (cdr mac-spec))
             (eq 'system::macro (cadr mac-spec)))
   collect (cons (car mac-spec) (cddr mac-spec))))

(defmethod lexical-symbol-macros ((environment c::lexenv))
  (loop
   for mac-spec in (c::lexenv-variables environment)
   when (and (consp (cdr mac-spec))
             (eq 'system::macro (cadr mac-spec)))
   collect (cons (car mac-spec) (cddr mac-spec))))

(defmethod augment-with-variable ((env c::lexenv) var)
  (c::make-lexenv :default env
                  :variables (list (cons var (c::make-lambda-var :name var)))))

(defmethod augment-with-function ((env c::lexenv) fun)
  (c::make-lexenv :default env
                  :functions (list (cons fun (lambda () 42)))))

(defmethod augment-with-macro ((env c::lexenv) mac def)
  (c::make-lexenv :default env
                  :functions (list (list* mac 'system::macro def))))

(defmethod augment-with-symbol-macro ((env c::lexenv) symmac def)
  (c::make-lexenv :default env
                  :variables (list (list* symmac 'system::macro def))))

) ; #+cmu

;;;; ** CLISP

#+clisp
(progn

(defun walk-vector-tree (function vector-tree)
  (labels ((%walk (vector-tree)
             (loop
                for index upfrom 0 by 2
                for tree-top = (aref vector-tree index)
                if (null tree-top)
                  do (return-from %walk nil)
                else if (vectorp tree-top)
                  do (return-from %walk
                       (%walk tree-top))
                else
                  do (funcall function
                              (aref vector-tree index)
                              (aref vector-tree (1+ index))))))
    (%walk vector-tree)))

(defmethod lexical-variables ((environment vector))
  (let ((vars '()))
    (when (aref environment 0)
      (walk-vector-tree (lambda (var-name var-spec)
                          (unless (system::symbol-macro-p var-spec)
                            (push var-name vars)))
                        (aref environment 0)))
    vars))

(defmethod lexical-functions ((environment vector))
  (let ((vars '()))
    (when (aref environment 1)
      (walk-vector-tree (lambda (func-name func-spec)
                          (push func-name vars))
                        (aref environment 1)))
    vars))

(defmethod lexical-macros ((environment vector))
  (let ((macros '()))
    (when (aref environment 1)
      (walk-vector-tree
       (lambda (macro-name macro-spec)
         (if (system::macrop macro-spec)
             (push (cons macro-name
                         (macro-function macro-name environment))
                   macros)))
       (aref environment 1)))
    macros))

(defmethod lexical-symbol-macros ((environment vector))
  (let (symbol-macros '())
    (when (aref environment 0)
      (walk-vector-tree
       (lambda (macro-name macro-spec)
         (if (system::symbol-macro-p macro-spec)
             (push (cons macro-name
                         (macroexpand-1 macro-name environment))
                   symbol-macros)))
       (aref environment 0)))
    symbol-macros))

(defun augment-with-var-and-fun (env &key var fun)
  (let* ((old-vars (aref env 0))
         (old-funs (aref env 1))
         (new-vars (if var
                       (make-array 3 :initial-contents (list (car var) (cdr var) old-vars))
                       (make-array 1 :initial-contents (list old-vars))))
         (new-funs (if fun
                       (make-array 3 :initial-contents (list (car fun) (cdr fun) old-funs))
                       (make-array 1 :initial-contents (list old-funs)))))
    (make-array 2 :initial-contents (list new-vars new-funs))))

;; I don't know whether T is an acceptable value to store here, but
;; CLISP does not complain.
(defmethod augment-with-variable ((env vector) var)
  (augment-with-var-and-fun env :var (cons var t)))

(defmethod augment-with-function ((env vector) fun)
  (augment-with-var-and-fun env :fun (cons fun t)))

(defmethod augment-with-macro ((env vector) mac def)
  (augment-with-var-and-fun env :fun (cons mac (system::make-macro def))))

(defmethod augment-with-symbol-macro ((env vector) symmac def)
  (augment-with-var-and-fun env :var (cons symmac (system::make-symbol-macro def))))

) ; #+clisp

;;;; ** LispWorks

#+(and lispworks macosx)
(progn

(defmethod lexical-variables ((environment system::augmented-environment))
  (mapcar (lambda (venv)
            (slot-value venv 'compiler::name))
          (remove-if (lambda (venv)
                       ;; regular variables, the ones we're interested
                       ;; in, appear to have a NIL in this slot.
                       (slot-value venv 'compiler::kind))
                     (slot-value environment 'compiler::venv))))

(defmethod lexical-functions ((environment system::augmented-environment))
  (mapcar #'car
          (remove-if (lambda (fenv)
                       ;; remove all the macros
                       (eql 'compiler::macro (slot-value (cdr fenv) 'compiler::function-or-macro)))
                     (slot-value environment 'compiler::fenv))))

(defmethod lexical-variables ((environment compiler::environment))
  (mapcar (lambda (venv)
            (slot-value venv 'compiler::name))
          (remove-if (lambda (venv)
                       ;; regular variables, the ones we're interested
                       ;; in, appear to have a NIL in this slot.
                       (slot-value venv 'compiler::kind))
                     (slot-value environment 'compiler::venv))))

(defmethod lexical-functions ((environment compiler::environment))
  (mapcar #'car
          (remove-if (lambda (fenv)
                       ;; remove all the macros
                       (macro-function (car fenv) environment))
                     (slot-value environment 'compiler::fenv))))

) ; #+(and lispworks macosx)

#+(and lispworks (or win32 linux))
(progn

(defun lexical-runtime-p (value)
  (and (symbolp value)
       (eq (symbol-package value) nil)))

(defmethod lexical-variables ((environment lexical::environment))
  (loop for candidate in (slot-value environment 'lexical::variables)
        if (lexical-runtime-p (cdr candidate))
        collect (car candidate)))

(defmethod lexical-functions ((environment lexical::environment))
  (loop for candidate in (slot-value environment 'lexical::functions)
        if (lexical-runtime-p (cdr candidate))
        collect (car candidate)))


(defmethod lexical-symbol-macros ((environment lexical::environment))
  (loop for candidate in (slot-value environment 'lexical::variables)
        unless (lexical-runtime-p (cdr candidate))
        collect candidate))

(defmethod lexical-macros ((environment lexical::environment))
  (loop for candidate in (slot-value environment 'lexical::functions)
        unless (lexical-runtime-p (cdr candidate))
        collect candidate))

(defmethod augment-with-variable ((env lexical::environment) var)
  (harlequin-common-lisp:augment-environment
   env :variable (list var)))

(defmethod augment-with-function ((env lexical::environment) fun)
  (harlequin-common-lisp:augment-environment
   env :function (list fun)))

(defmethod augment-with-macro ((env lexical::environment) mac def)
  (harlequin-common-lisp:augment-environment
   env :macro (list (list mac def))))

(defmethod augment-with-symbol-macro ((env lexical::environment) symmac def)
  (harlequin-common-lisp:augment-environment
   env :symbol-macro (list (list symmac def))))

) ; #+(and lispworks (or win32 linux))

;;;; ** Allegro

#+(and allegro (version>= 7 0))
(progn

(defmethod lexical-variables ((env sys::augmentable-environment))
  (let (fns)
    (system::map-over-environment-variables
     (lambda (symbol type rest)
       (declare (ignore rest))
       (when (and (eq type :lexical)
                  (sys:variable-information symbol env))
         (push symbol fns)))
     env)
    fns))

(defmethod lexical-functions ((env sys::augmentable-environment))
  (let (fns)
    (system::map-over-environment-functions
     (lambda (name type rest)
       (when (and (eq type :function)
                  (sys:function-information name env))
         (push name fns)))
     env)
    fns))

(defmethod lexical-macros ((env sys::augmentable-environment))
  (let (fns)
    (system::map-over-environment-functions
     (lambda (name type rest)
       (when (eq type :macro)
         (push (cons name (car rest)) fns)))
     env)
    fns))

(defmethod lexical-symbol-macros ((env sys::augmentable-environment))
  (let (fns)
    (system::map-over-environment-variables
     (lambda (symbol type rest)
       (when (eq type :symbol-macro)
         (push (cons symbol (car rest)) fns)))
     env)
    fns))

(defmethod augment-with-variable ((env sys::augmentable-environment) var)
  (system:augment-environment env :variable (list var)))

(defmethod augment-with-function ((env sys::augmentable-environment) fun)
  (system:augment-environment env :function (list fun)))

(defmethod augment-with-macro ((env sys::augmentable-environment) mac def)
  (system:augment-environment env :macro (list (list mac def))))

(defmethod augment-with-symbol-macro ((env sys::augmentable-environment) symmac def)
  (system:augment-environment env :symbol-macro (list (list symmac def))))

) ; #+(and allegro (version>= 7 0))

||#

