;;;; -*- lisp -*-

(in-package :it.bese.arnesi)

;;;; * Portable lexical environment access

;;;; ** OpenMCL

#+openmcl
(defun environment-p (environment)
  (subtypep (class-of environment) 'ccl::lexical-environment))

#+openmcl
(defun lexical-variables (environment)
  (loop
     for env = environment
          then (ccl::lexenv.parent-env env)
     while (and env
                (not (ccl::istruct-typep env 'ccl::definition-environment)))
     for vars = (ccl::lexenv.variables env)
     when (listp vars)
     append (mapcar (lambda (var)
                      ;; ccl::var-name is a macro, se we can't do #'ccl::var-name directly
                      (ccl::var-name var))
                    vars)))  

;fixme 
#+openmcl
(defun lexical-functions (environment)
  (declare (ignore environment))
  nil)

;;;; ** SBCL
 
#+sbcl
(defun environment-p (environment)
  (subtypep (class-of environment) (find-class 'sb-kernel:lexenv)))

#+sbcl
(defun lexical-variables (environment)
  (mapcar #'first (sb-c::lexenv-vars environment)))

#+sbcl
(defun lexical-functions (environment)
  (mapcar #'first (sb-c::lexenv-funs environment)))


;;;; ** CMUCL

#+cmu
(defun environment-p (environment)
  (subtypep (class-of environment) (find-class 'c::lexenv)))

#+cmu
(defun lexical-variables (environment)
  (mapcar #'first (c::lexenv-variables environment)))

;fixme 
#+cmu
(defun lexical-functions (environment)
  (declare (ignore environment))
  nil)


