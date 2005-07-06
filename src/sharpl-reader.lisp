;; -*- lisp -*-

(in-package :arnesi)

;;;; * A reader macro for simple lambdas

(defun enable-sharp-l ()
  (set-dispatch-macro-character #\# #\L #'sharpL-reader))

(defun sharpL-reader (stream subchar min-args)
  "Reader macro for simple lambdas."
  (declare (ignore subchar))
  (let* ((form (read stream t nil t))
         (lambda-args (loop
                         for i upfrom 1 upto (max (or min-args 0)
                                                  (highest-bang-var form))
                         collect (make-sharpl-arg i))))
    `(lambda ,lambda-args
       , (when lambda-args
           `(declare (ignorable ,@lambda-args)))
       ,form)))

(defun highest-bang-var (form)
  (acond
   ((consp form) (max (highest-bang-var (car form))
                      (highest-bang-var (cdr form))))
   ((bang-var-p form) it)
   (t 0)))

(defun bang-var-p (form)
  (and (symbolp form)
       (char= #\! (aref (symbol-name form) 0))
       (parse-integer (subseq (symbol-name form) 1) :junk-allowed t)))

(defun make-sharpl-arg (number)
  (intern (format nil "!~D" number)))

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

;; This code was heavily inspired by iterate, which has the following
;; copyright:

;;                     ITERATE, An Iteration Macro
;;
;;                 Copyright 1989 by Jonathan Amsterdam
;;         Adapted to ANSI Common Lisp in 2003 by Andreas Fuchs
;;
;; Permission to use, copy, modify, and distribute this software and its
;; documentation for any purpose and without fee is hereby granted,
;; provided that this copyright and permission notice appear in all
;; copies and supporting documentation, and that the name of M.I.T. not
;; be used in advertising or publicity pertaining to distribution of the
;; software without specific, written prior permission. M.I.T. makes no
;; representations about the suitability of this software for any
;; purpose.  It is provided "as is" without express or implied warranty.

;; M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
;; ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
;; M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
;; ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
;; WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
;; ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
;; SOFTWARE.
