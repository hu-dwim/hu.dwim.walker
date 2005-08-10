;; -*- lisp -*-

(in-package :arnesi)

;;;; FUNCTION and LAMBDA form

(defmethod evaluate/cc ((func free-function-object-form) env k)
  (declare (ignore env))
  (if (fboundp (name func))
      (kontinue k (fdefinition (name func)))
      (error "Unbound function ~S." (name func))))

(defmethod evaluate/cc ((func local-function-object-form) env k)
  (kontinue k (lookup env :flet (name func) :error-p t)))

(defclass closure/cc ()
  ((code :accessor code :initarg :code)
   (env :accessor env :initarg :env))
  #+sbcl (:metaclass mopp:funcallable-standard-class))

#+sbcl
(defmethod initialize-instance :after ((fun closure/cc) &rest initargs)
  (declare (ignore initargs))
  (mopp:set-funcallable-instance-function 
   fun 
   #'(lambda (&rest args)
       (drive-interpreter/cc 
	(apply-lambda/cc fun
			 args
			 *toplevel-k*)))))

(defmethod evaluate/cc ((lambda lambda-function-form) env k)
  (kontinue k (make-instance 'closure/cc :code lambda :env env)))

(defk k-for-call/cc (k)
    (value)
  (if *call/cc-returns*
      (kontinue k value)
      (throw 'done value)))

(defmethod evaluate/cc ((func free-application-form) env k)
  (cond 
    ((eql 'call/cc (operator func))
     (evaluate/cc (make-instance 'free-application-form
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

    ((and (eql 'call-next-method (operator func))
	  (second (multiple-value-list (lookup env :next-method t))))
     (aif (lookup env :next-method t)
	  (evaluate-arguments-then-apply
	   (lambda (arguments)
	     (apply-lambda/cc it arguments k))
	   (arguments func) '() env)
	  (error "no next method")))

    ((and (eql 'next-method-p (operator func))
	  (second (multiple-value-list (lookup env :next-method t))))
     (kontinue k (lookup env :next-method t)))
    
    ((eql 'funcall (operator func))
     (evaluate-funcall/cc (arguments func) env k))
    
    ((eql 'apply (operator func))
     (evaluate-apply/cc (arguments func) '() env k))
    
    ((and (symbolp (operator func))
          (eql 'defun/cc (nth-value 1 (fdefinition/cc (operator func)))))
     (evaluate-arguments-then-apply
      (lambda (arguments)
        (apply-lambda/cc (fdefinition/cc (operator func)) arguments k))
      (arguments func) '()
      env))

    ((and (symbolp (operator func))
          (eql 'defmethod/cc (nth-value 1 (fdefinition/cc (operator func)))))
     (evaluate-arguments-then-apply
      (lambda (arguments)
        (apply-lambda/cc (apply (operator func) arguments) arguments k))
      (arguments func) '()
      env))
       
    (t
     (evaluate-arguments-then-apply
      (lambda (arguments)
        (apply #'kontinue k (multiple-value-list (apply (fdefinition (operator func)) arguments))))
      (arguments func) '()
      env))))

(defmethod evaluate/cc ((func local-application-form) env k)
  (evaluate-apply/cc (arguments func) (list (list (lookup env :flet (operator func) :error-p t))) env k))

(defk k-for-apply/cc (remaining-arguments evaluated-arguments env k)
    (value)
  (evaluate-apply/cc (cdr remaining-arguments) (cons value evaluated-arguments)
                     env k))

(defun evaluate-apply/cc (remaining-arguments evaluated-arguments env k)
  (if remaining-arguments
      (evaluate/cc (car remaining-arguments) env
                   `(k-for-apply/cc ,remaining-arguments ,evaluated-arguments ,env ,k))
      (let ((arg-list (apply #'list* (reverse evaluated-arguments))))
        (apply-lambda/cc (first arg-list) (rest arg-list) k))))

(defun evaluate-funcall/cc (arguments env k)
  (evaluate-apply/cc (append (butlast arguments)
                             (list (make-instance 'free-application-form
                                                  :operator 'list
                                                  :source `(list ,(source (car (last arguments))))
                                                  :arguments (last arguments))))
                     '()
                     env k))

(defmethod apply-lambda/cc ((operator closure/cc) effective-arguments k)
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
        (rest-function-argument-form
	 (setf env (register env :let (name parameter) remaining-arguments))
	 (setf remaining-arguments nil))
	(optional-function-argument-form
         (if remaining-arguments
             (progn
               (setf env (register env :let (name parameter) (pop remaining-arguments)))
               (when (supplied-p-parameter parameter)
                 (setf env (register env :let (supplied-p-parameter parameter) t))))
             (return-from apply-lambda/cc
               ;; we need to evaluate a default-value, since this may
               ;; contain call/cc we need to setup the continuation
               ;; and let things go from there (hence the return-from)
               (evaluate/cc (default-value parameter) env
                            `(k-for-apply/cc/optional-argument-default-value
                              ;; remaining-arguments is, by
                              ;; definition, NIL so we needn't pass
                              ;; it here.
                              ,operator ,remaining-parameters ,env ,k)))))))
    
    (evaluate-progn/cc (body (code operator)) env k)))

(defk k-for-apply/cc/optional-argument-default-value
    (operator remaining-parameters env k)
    (value)
  (apply-lambda/cc/optional-default-value
   operator (cdr remaining-parameters)
   (register env :let (name (first remaining-parameters)) value) k))

(defun apply-lambda/cc/optional-default-value (operator remaining-parameters env k)
  (if remaining-parameters
      (dolist (parameter remaining-parameters)
        (typecase parameter
          (optional-function-argument-form
           (return-from apply-lambda/cc/optional-default-value
             (evaluate/cc (default-value parameter) env
                          `(k-for-apply/cc-optional-argument-default-value
                            ,operator ,(cdr remaining-parameters) ,env ,k))))
          (t (error "Sorry, only required and optional arguments for now."))))
      (evaluate-progn/cc (body (code operator)) env k)))

(defmethod apply-lambda/cc ((operator function) effective-arguments k)
  (apply #'kontinue k (multiple-value-list (apply operator effective-arguments))))

(defk k-for-evaluate-arguments-then-apply (handler remaining-arguments evaluated-arguments env)
    (value)
  (evaluate-arguments-then-apply
   handler
   remaining-arguments (cons value evaluated-arguments)
   env))

(defun evaluate-arguments-then-apply (handler remaining-arguments evaluated-arguments env)
  (if remaining-arguments
      (evaluate/cc (car remaining-arguments) env
                    `(k-for-evaluate-arguments-then-apply ,handler ,(cdr remaining-arguments)
                                                          ,evaluated-arguments ,env))
      (funcall handler (reverse evaluated-arguments))))

;;;; FUNCTION and LAMBDA application

(defmethod evaluate/cc ((lambda lambda-application-form) env k)
  (evaluate-funcall/cc (cons (operator lambda) (arguments lambda)) env k))

;; Copyright (c) 2002-2005, Edward Marco Baringer
;; All rights reserved. 
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;; 
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;    of its contributors may be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
