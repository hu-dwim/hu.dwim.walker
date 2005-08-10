;; -*- lisp -*-

(in-package :it.bese.arnesi)

;;;; ** Handlres for common-lisp special operators

;;;; Variable References

(defmethod evaluate/cc ((var local-variable-reference) env k)
  (kontinue k (lookup env :let (name var) :error-p t)))

(defmethod evaluate/cc ((var local-lexical-variable-reference) env k)
  (kontinue k (funcall (first (lookup env :lexical-let (name var) :error-p t)))))

(defmethod evaluate/cc ((var free-variable-reference) env k)
  (declare (ignore env))
  (kontinue k (symbol-value (name var))))

;;;; Constants

(defmethod evaluate/cc ((c constant-form) env k)
  (declare (ignore env))
  (kontinue k (value c)))

;;;; BLOCK/RETURN-FROM

(defmethod evaluate/cc ((block block-form) env k)
  (evaluate-progn/cc (body block) (register env :block (name block) k) k))

(defmethod evaluate/cc ((return return-from-form) env k)
  (declare (ignore k))
  (evaluate/cc (result return) env (lookup env :block (name (target-block return)) :error-p t)))

;;;; CATCH/THROW

(defk catch-tag-k (catch env k)
    (tag)
  (evaluate-progn/cc (body catch) (register env :catch tag k) k))

(defmethod evaluate/cc ((catch catch-form) env k)
  (evaluate/cc (tag catch) env `(catch-tag-k ,catch ,env ,k)))

(defk throw-tag-k (throw env k)
    (tag)
  (evaluate/cc (value throw) env (lookup env :catch tag :error-p t)))

(defmethod evaluate/cc ((throw throw-form) env k)
  (evaluate/cc (tag throw) env
                `(throw-tag-k ,throw ,env ,k)))

;;;; FLET/LABELS

(defmethod evaluate/cc ((flet flet-form) env k)
  (let ((new-env env))
    (dolist* ((name . form) (binds flet))
      (setf new-env (register new-env :flet name (make-instance 'closure/cc
                                                                :code form
                                                                :env env))))
    (evaluate-progn/cc (body flet) new-env k)))

(defmethod evaluate/cc ((labels labels-form) env k)
  (let ((closures '()))
    (dolist* ((name . form) (binds labels))
      (let ((closure (make-instance 'closure/cc :code form)))
        (setf env (register env :flet name closure))
        (push closure closures)))
    (dolist (closure closures)
      (setf (env closure) env))
    (evaluate-progn/cc (body labels) env k)))

;;;; LET/LET*

(defmethod evaluate/cc ((let let-form) env k)
  (evaluate-let/cc (binds let) nil (body let) env k))

(defk k-for-evaluate-let/cc (var remaining-bindings evaluated-bindings body env k)
    (value)
  (evaluate-let/cc remaining-bindings
                   (cons (cons var value) evaluated-bindings)
                   body env k))

(defun evaluate-let/cc (remaining-bindings evaluated-bindings body env k)
  (if remaining-bindings
      (destructuring-bind (var . initial-value)
          (car remaining-bindings)
        (evaluate/cc
         initial-value env
                      `(k-for-evaluate-let/cc
                        ,var
                        ,(cdr remaining-bindings)
                        ,evaluated-bindings
                        ,body
                        ,env
                        ,k)))
      (dolist* ((var . value) evaluated-bindings
                (evaluate-progn/cc body env k))
        (setf env (register env :let var value)))))

(defmethod evaluate/cc ((let* let*-form) env k)
  (evaluate-let*/cc (binds let*) (body let*) env k))

(defk k-for-evaluate-let*/cc (var bindings body env k)
    (value)
  (evaluate-let*/cc bindings body
                     (register env :let var value) k))

(defun evaluate-let*/cc (bindings body env k)
  (if bindings
      (destructuring-bind (var . initial-value)
          (car bindings)
        (evaluate/cc initial-value env
                      `(k-for-evaluate-let*/cc ,var ,(cdr bindings) ,body ,env ,k)))
      (evaluate-progn/cc body env k)))

;;;; IF

(defk k-for-evaluate-if/cc (then else env k)
    (value)
  (if value
      (evaluate/cc then env k)
      (evaluate/cc else env k)))

(defmethod evaluate/cc ((if if-form) env k)
  (evaluate/cc (consequent if) env
                `(k-for-evaluate-if/cc ,(then if) ,(else if) ,env ,k )))

;;;; LOCALLY

(defmethod evaluate/cc ((locally locally-form) env k)
  (evaluate-progn/cc (body locally) env k))

;;;; MACROLET

(defmethod evaluate/cc ((macrolet macrolet-form) env k)
  (evaluate-progn/cc (body macrolet) env k))

;;;; multiple-value-call

(defk k-for-m-v-c (remaining-arguments evaluated-arguments env k)
    (value other-values)
  (evaluate-m-v-c
   remaining-arguments (append evaluated-arguments (list value) other-values)
   env k))

(defun evaluate-m-v-c (remaining-arguments evaluated-arguments env k)
  (if remaining-arguments
      (evaluate/cc (car remaining-arguments) env
                    `(k-for-m-v-c  ,(cdr remaining-arguments) ,evaluated-arguments ,env ,k))
      (destructuring-bind (function &rest arguments)
          evaluated-arguments
        (etypecase function
          (closure/cc (apply-lambda/cc function evaluated-arguments k))
          (function (apply #'kontinue k (multiple-value-list
                                         (multiple-value-call function (values-list arguments)))))))))

(defmethod evaluate/cc ((m-v-c multiple-value-call-form) env k)
  (evaluate-m-v-c (list* (func m-v-c) (arguments m-v-c)) '() env k))

;;;; PROGN

(defmethod evaluate/cc ((progn progn-form) env k)
  (evaluate-progn/cc (body progn) env k))

(defk k-for-evaluate-progn/cc (rest-of-body env k)
    ()
  (evaluate-progn/cc rest-of-body env k))

(defun evaluate-progn/cc (body env k)
  (cond
    ((cdr body)
      (evaluate/cc (first body) env
                    `(k-for-evaluate-progn/cc ,(cdr body) ,env ,k)))
    (body
     (evaluate/cc (first body) env k))
    (t
     (kontinue k nil))))

;;;; SETQ

(defk k-for-local-setq (var env k)
    (value)
  (setf (lookup env :let var :error-p t) value)
  (kontinue k value))

(defk k-for-free-setq (var env k)
    (value)
  (setf (symbol-value var) value)
  (kontinue k value))

(defk k-for-local-lexical-setq (var env k)
    (value)
  (funcall (second (lookup env :lexical-let var :error-p t)) value)
  (kontinue k value))

(defmethod evaluate/cc ((setq setq-form) env k)
  (multiple-value-bind (value foundp)
      (lookup env :let (var setq))
    (declare (ignore value))
    (if foundp
        (evaluate/cc (value setq)
                      env `(k-for-local-setq ,(var setq) ,env ,k))
        (multiple-value-bind (value foundp)
            (lookup env :lexical-let (var setq))
          (declare (ignore value))
          (if foundp
              (evaluate/cc (value setq)
                            env `(k-for-local-lexical-setq ,(var setq) ,env ,k))
              (evaluate/cc (value setq)
                            env `(k-for-free-setq ,(var setq) ,env ,k)))))))

;;;; SYMBOL-MACROLET

(defmethod evaluate/cc ((symbol-macrolet symbol-macrolet-form) env k)
  (evaluate-progn/cc (body symbol-macrolet) env k))

;;;; TAGBODY/GO

(defk tagbody-k (k)
    ()
  (kontinue k nil))

(defmethod evaluate/cc ((tagbody tagbody-form) env k)
  (evaluate-progn/cc (body tagbody) (register env :tag tagbody k) `(tagbody-k ,k)))

(defmethod evaluate/cc ((go-tag go-tag-form) env k)
  (declare (ignore go-tag env))
  (kontinue k nil))

(defmethod evaluate/cc ((go go-form) env k)
  (declare (ignore k))
  (evaluate-progn/cc (target-progn go) env (lookup env :tag (enclosing-tagbody go) :error-p t)))

;;;; THE

(defmethod evaluate/cc ((the the-form) env k)
  (evaluate/cc (value the) env k))

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
