;;;; -*- lisp -*-

(in-package :it.bese.arnesi)

;;;; * Portable lexical environment access

;;;; ** OpenMCL

#+openmcl
(defun environment-p (environment)
  (subtypep (class-of environment) 'ccl::lexical-environment))

#+openmc
(defun lexical-variables (environment)
  (loop
     for env = environment then (ccl::lexenv.parent-env env)
     while env
     for vars = (ccl::lexenv.variables env)
     when (listp vars)
     append (mapcar (lambda (var)
                      ;; ccl::var-name is a macro, se we can't do #'ccl::var-name directly
                      (ccl::var-name var))
                    vars)))  
  

;;;; ** SBCL
 
#+sbcl
(defun environment-p (environment)
  (subtypep (class-of environment) (find-class 'sb-kernel:lexenv)))

#+sbcl
(defun lexical-variables (environment)
  (mapcar #'first (sb-c::lexenv-vars environment)))

;;;; ** CMUCL

#+cmu
(defun environment-p (environment)
  (subtypep (class-of environment) (find-class 'c::lexenv)))

#+cmu
(defun lexical-variables (environment)
  (mapcar #'first (c::lexenv-variables environment)))

