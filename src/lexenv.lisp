;;;; -*- lisp -*-

(in-package :it.bese.arnesi)

;;;; * Portable lexical environment access

(defgeneric environment-p (environment)
  (:documentation "Returns T if ENVIRONMENT is a lexical
  environment object (something suitable for passing to
  macroexpand-1 or similar)"))

(defgeneric lexical-variables (environment)
  (:documentation "Return the names of all the local variables
  in ENVIRONMENT. Does not return neither symbol-macrolets nor
  ignared variables."))

(defgeneric lexical-functions (environment)
  (:documentation "Returns the names of all the local functions
  in ENVIRONMENT. Names may be symbols of lists of the form (setf
  name)."))

(defmethod lexical-variables ((environment null))
  '())

(defmethod lexical-functions ((environment null))
  '())

;;;; ** OpenMCL

#+openmcl
(defmethod environment-p ((e ccl::lexical-environment))
  t)

#+openmcl
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

#+openmcl
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

;;;; ** SBCL
 
#+sbcl
(defmethod environment-p ((environment sb-kernel:lexenv))
  t)

#+sbcl
(defmethod lexical-variables ((environment sb-kernel:lexenv))
  (loop
     for var-spec in (sb-c::lexenv-vars environment)
     when (and (atom (cdr var-spec))
               (not (and (typep (cdr var-spec) 'sb-c::lambda-var)
			 (sb-c::lambda-var-ignorep (cdr var-spec)))))
     collect (car var-spec)))

#+sbcl
(defmethod lexical-functions ((environment sb-kernel:lexenv))
  (mapcar #'first (sb-c::lexenv-funs environment)))

;;;; ** CMUCL

#+cmu
(defmethod environment-p ((environment c::lexenv))
  t)

#+cmu
(defmethod lexical-variables ((environment c::lexenv))
  (loop
     for var-spec in (c::lexenv-variables environment)
     ;; variable refs are (NAME . LAMBDA-VAR), we want to void
     ;; symbol-macrolets which are (NAME SYSTEM:MACRO . EXPANSION)
     when (and (atom (cdr var-spec))
               ;; don't return ignored vars
               (not (c::lambda-var-ignorep (cdr var-spec))))
       collect (car var-spec)))

#+cmu
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
     ;; if we get here we're confused :(
     else
       do (error "Sorry, don't know how to handle the lexcial function spec ~S."
                 func-spec)))

;;;; ** CLISP

#+clisp
(defmethod environment-p ((environment vector))
  (= 2 (length environment)))

#+clisp
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

#+clisp
(defmethod lexical-variables ((environment vector))
  (let ((vars '()))
    (when (aref environment 0)
      (walk-vector-tree (lambda (var-name var-spec)
                          (unless (system::symbol-macro-p var-spec)
                            (push var-name vars)))
                        (aref environment 0)))
    vars))

#+clisp
(defmethod lexical-functions ((environment vector))
  (let ((vars '()))
    (when (aref environment 1)
      (walk-vector-tree (lambda (func-name func-spec)
                          (push func-name vars))
                        (aref environment 1)))
    vars))

;;;; ** LispWorks

#+lispworks
(defmethod environment-p ((environment system::augmented-environment))
  t)

#+lispworks
(defmethod lexical-variables ((environment system::augmented-environment))
  (mapcar (lambda (venv)
            (slot-value venv 'compiler::name))
          (remove-if (lambda (venv)
                       ;; regular variables, the ones we're interested
                       ;; in, appear to have a NIL in this slot.
                       (slot-value venv 'compiler::kind))
                     (slot-value environment 'compiler::venv))))

#+lispworks
(defmethod lexical-functions ((environment system::augmented-environment))
  (mapcar #'car
          (remove-if (lambda (fenv)
                       ;; remove all the macros
                       (eql 'compiler::macro (slot-value (cdr fenv) 'compiler::function-or-macro)))
                     (slot-value environment 'compiler::fenv))))
