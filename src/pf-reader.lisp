;; -*- lisp -*-

(in-package :it.bese.arnesi)

;;;; * A partial application syntax

;;;; Reader
(defun enable-pf-reader (&optional (readtable *readtable*))
  (set-macro-character  #\[ #'|[-reader| t readtable)
  (set-syntax-from-char #\] #\) readtable))

(defun |[-reader| (stream char)
  (declare (ignore char))
  (destructuring-bind (fname &rest args)
      (read-delimited-list #\] stream t)
    (let* ((rest  (gensym "REST"))
	   (count (count '_ args))
	   (end   (if (zerop count) rest `(nthcdr ,count ,rest)))
	   (args  (reduce (lambda (x y)
			    (cons (if (eq x '_)
				      `(nth ,(decf count) ,rest)
				      x)
				  y))
			  args
			  :from-end t
			  :initial-value '())))
      `(lambda (&rest ,rest) (apply #',fname ,@args ,end)))))

;;;; http://groups.google.com/group/comp.lang.lisp/browse_thread/thread/1a86740db77b2f3a

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
