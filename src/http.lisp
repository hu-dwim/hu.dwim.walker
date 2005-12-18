;; -*- lisp -*-

(in-package :it.bese.arnesi)

;;;; * HTTP/HTML utilities

;;;; ** URIs/URLs

(defvar *ok-set*
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.,"
  "The list of characters which don't need to be escaped when
  writing URIs.")

(defun escape-as-uri (string)
  "Escapes all non alphanumeric characters in STRING following
  the URI convention. Returns a fresh string."
  (with-output-to-string (escaped)
    (write-as-uri string escaped)))

(defun write-as-uri (string stream)
  (loop
     for char across string
     if (find char *ok-set* :test #'char=)
       do (write-char char stream)
     else do (format stream "%~2,'0X" (char-code char))))

(declaim (inline char->hex-value))
(defun char->hex-value (char)
  "Returns the number associated the hex value of CHAR. CHAR must
  be one of #\0 - #\9, #\a - #\f, #\A - #\F."
  (ecase char
    (#\0 0)
    (#\1 1)
    (#\2 2)
    (#\3 3)
    (#\4 4)
    (#\5 5)
    (#\6 6)
    (#\7 7)
    (#\8 8)
    (#\9 9)
    ((#\a #\A) 10)
    ((#\b #\B) 11)
    ((#\c #\C) 12)
    ((#\d #\D) 13)
    ((#\e #\E) 14)
    ((#\f #\F) 15)))

(defun make-escaped-table ()
  (let ((table (make-array '(16 16)
                           :element-type 'character
                           :initial-element #\\)))
    (dotimes (i 16)
      (dotimes (j 16)
        (setf (aref table i j) (code-char (+ (* i 16) j)))))
    table))

(defvar *unescape-table* (make-escaped-table))

(defun nunescape-as-uri (string &rest args)
  (apply #'unescape-as-uri string args))

(defun unescape-as-uri (string &optional (external-format :latin-1))
  (declare (ignorable external-format))
  (flet ((unescape-to-bytes (string)
           (let* ((length (- (length string)
                             (* 2 (count #\% string :test #'char=))))
                  (result (make-array length :element-type '(unsigned-byte 8))))
             (loop
                for index1 upfrom 0
                for index2 upfrom 0
                while (< index1 (length string))
                do (setf (aref result index2)
                         (case (aref string index1)
                           (#\% (+ (ash (digit-char-p (aref string (incf index1)) 16) 4)
                                   (digit-char-p (aref string (incf index1)) 16)))
                           (#\+ #.(char-code #\space))
                           (t (char-code (aref string index1))))))
             result)))
    (octets-to-string (unescape-to-bytes string)
                      external-format))) 

;;;; ** HTML

;;;; This so blatently wrong its not even funny, and while this is
;;;; exactly what I need I would do well to start using a "real" html
;;;; escaping library (there are a couple to choose from).

(defun make-html-entities ()
  (let ((ht (make-hash-table :test 'equal)))
    (flet ((add-mapping (char escaped)
             (setf (gethash char ht) escaped
                   (gethash escaped ht) char)))
      (add-mapping #\< "&lt;")
      (add-mapping #\> "&gt;")
      (add-mapping #\& "&amp;")
      (add-mapping #\" "&quot;")
      (add-mapping "a`" "&#224;")
      (add-mapping "a'" "&#225;")
      (add-mapping "e`" "&#232;")
      (add-mapping "e'" "&#233;")
      (add-mapping "i'" "&#236;")
      (add-mapping "i`" "&#237;")
      (add-mapping "o`" "&#242;")
      (add-mapping "o'" "&#243;")
      (add-mapping "u`" "&#249;")
      (add-mapping "u'" "&#250;"))
    ht))

(defparameter *html-entites* (make-html-entities))

(defun write-as-html (string &key (stream t) (escape-whitespace nil))
  (loop
     for char across string
     do (cond
          ((char= char #\Space)
           (if escape-whitespace
               (princ "&nbsp;" stream)
               (write-char char stream)))
          ((gethash char *html-entites*)
           (princ (gethash char *html-entites*) stream))
          (t (write-char char stream)))))

(defun escape-as-html (string &key (escape-whitespace nil))
  (with-output-to-string (escaped)
    (write-as-html string
                   :stream escaped
                   :escape-whitespace escape-whitespace))) 

(define-condition html-escape-error (error)
  ((what :accessor html-escape-error.what :initarg :what)))

(define-condition unterminated-html-entity (html-escape-error)
  ())

(define-condition unknown-html-entity (html-escape-error)
  ())

(define-condition unknown-char-escape (warning)
  ((what :accessor html-escape-error.what :initarg :what)))

(defun unescape-as-html (string)
  (with-output-to-string (unescaped)
    (loop
       for offset upfrom 0 below (length string)
       for char = (aref string offset)
       if (char= #\& char)
         do (progn
              (aif (position #\; string :start offset)
                   (let ((escape-tag (subseq string offset (1+ it))))
                     (aif (gethash escape-tag *html-entites*)
                          (progn
                            (princ it unescaped)
                            (incf offset (1- (length escape-tag))))
                          (if (char= #\# (aref escape-tag 1))
                              ;; special code, ignore
                              (restart-case
                                  (warn 'unknown-char-escape :what escape-tag)
                                (continue-delete ()
                                  :report "Continue processing, delete this char."
                                  (incf offset (1- (length escape-tag)))))
                              (restart-case
                                  (error 'unknown-html-entity :what escape-tag)
                                (continue-as-is ()
                                  :report "Continue processing, leaving the string as is."
                                  (write-char #\& unescaped))
                                (continue-delete ()
                                  :report "Continue processing, delete this entity."
                                  (incf offset (1- (length escape-tag))))))))
                   (restart-case
                       (error 'unterminated-html-entity
                              :what (subseq string offset
                                            (min (+ offset 20)
                                                 (length string))))
                     (continue-as-is ()
                       :report "Continue processing, leave the string as is."
                       (write-char #\& unescaped)))))
       else do (write-char char unescaped))))

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
