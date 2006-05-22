;; -*- lisp -*-

(in-package :it.bese.arnesi)

;;;; * Lisp-1 support

;;;; Entry point
(defgeneric lisp1 (form)
  (:documentation "Translate FORM from Lisp-1 to Lisp-2"))

(defmacro with-lisp1 (form)
  (unwalk-form (lisp1 (walk-form form))))

(defmacro deflisp1-walker (class (&rest slots) &body body)
  `(defmethod lisp1 ((form ,class))
     (with-slots ,slots form
       ,@body)))

;;;; Definers
(defmacro defun1 (name (&rest args) &body body)
  `(defun ,name ,args
     (with-lisp1 (progn ,@body))))

(defmacro defmethod1 (name (&rest args) &body body)
  `(defmethod ,name ,args
     (with-lisp1 (progn ,@body))))

;;;; Utils
(defun lisp1s (forms)
  (mapcar #'lisp1 forms))

(defun lisp1b (binds)
  (mapcar (lambda (bind)
	    (cons (car bind)
		  (lisp1 (cdr bind))))
	  binds))

(defvar *bound-vars* nil)

;;;; Walker
(deflisp1-walker form ()
  form)

(deflisp1-walker if-form (consequent then else)
  (new 'if-form
       :consequent (lisp1 consequent)
       :then       (lisp1 then)
       :else       (lisp1 else)))

(deflisp1-walker lambda-function-form (arguments body)
  (let1 *bound-vars*
      (append (mapcar #'name arguments) *bound-vars*)
    (new 'lambda-function-form
	 :arguments arguments
	 :body      (lisp1s body))))

(deflisp1-walker free-variable-reference (name)
  (if (and (fboundp name) (not (member name *bound-vars*)))
      (change-class form 'free-function-object-form)
      form))

(deflisp1-walker application-form (operator arguments)
  (new 'free-application-form
       :operator  'funcall
       :arguments (cons (if (not (typep operator 'form))
			    (lisp1 (walk-form operator))
			    (lisp1 operator))
			(lisp1s arguments))))

(deflisp1-walker function-binding-form (binds body)
  (new (class-name-of form)
       :binds (lisp1b binds)
       :body  (lisp1s body)))

(deflisp1-walker variable-binding-form (binds body)
  (new (class-name-of form)
       :binds (lisp1b binds)
       :body  (lisp1s body)))

(deflisp1-walker setq-form (var value)
  (new 'setq-form
       :var   var
       :value (lisp1 value)))

(deflisp1-walker progn-form (body)
  (new 'progn-form
       :body (lisp1s body)))

(deflisp1-walker progv-form (vars-form values-form)
  (new 'progv-form
       :vars-form   (lisp1s vars-form)
       :values-form (lisp1s values-form)))

(deflisp1-walker block-form (name body)
  (new 'block-form
       :name name
       :body (lisp1s body)))

(deflisp1-walker catch-form (tag body)
  (new 'catch-form
       :tag  tag
       :body (lisp1s body)))

(deflisp1-walker multiple-value-call-form (func arguments)
  (new 'multiple-value-call-form
       :func      (lisp1 func)
       :arguments (lisp1s arguments)))

(deflisp1-walker multiple-value-prog1-form (first-form other-forms)
  (new 'multiple-value-prog1-form
       :first-form  (lisp1 first-form)
       :other-forms (lisp1s other-forms)))

(deflisp1-walker symbol-macrolet-form (binds body)
  (new 'symbol-macrolet-form
       :binds (lisp1b binds)
       :body  (lisp1s body)))

(deflisp1-walker tagbody-form (body)
  (new 'tagbody-form
       :body (lisp1s body)))

(deflisp1-walker the-form (type-form value)
  (new 'the-form
       :type-form type-form
       :value     (lisp1 value)))

(deflisp1-walker unwind-protect-form (protected-form cleanup-form)
  (new 'unwind-protect-form
       :protected-form (lisp1 protected-form)
       :cleanup-form   (lisp1 cleanup-form)))

;;;; http://groups.google.com/group/comp.lang.lisp/browse_thread/thread/82994055009163e9

;; Copyright (c) 2006, Hoan Ton-That
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
;;  - Neither the name of Hoan Ton-That, nor the names of the
;;    contributors may be used to endorse or promote products derived
;;    from this software without specific prior written permission.
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
