;; -*- lisp -*-

(in-package :it.bese.arnesi)

;;;; * HTTP/HTML utilities

;;;; ** URIs/URLs

(eval-always
  (defvar *uri-escaping-ok-table* (make-array 256
                                              :element-type 'boolean
                                              :initial-element nil))
  (loop
      ;; The list of characters which don't need to be escaped when writing URIs.
      for ok-char across "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.," do
      (setf (aref *uri-escaping-ok-table* (char-code ok-char)) t)))

(defun escape-as-uri (string)
  "Escapes all non alphanumeric characters in STRING following
  the URI convention. Returns a fresh string."
  (with-output-to-string (escaped)
    (write-as-uri string escaped)))

(defun write-as-uri (string stream)
  (declare (type vector string)
           (type stream stream)
           (optimize (speed 3) (debug 0)))
  (loop
      for char across string
      for char-code = (char-code char) do
      (assert (<= char-code #16rffff))
      (cond ((and (< char-code 256)
                  (aref #.*uri-escaping-ok-table* char-code))
             (write-char char stream))
            ((< char-code 256)
             (format stream "%~2,'0X" char-code))
            (t
             (format stream "%u~4,'0X" char-code)))))

(defun unescape-as-uri (string)
  (declare (inline))
  (%unescape-as-uri string nil))

(defun nunescape-as-uri (string)
  (declare (inline))
  (%unescape-as-uri string t))

(defun %unescape-as-uri (input may-modify-input-p)
  (declare (type string input)
           (optimize (speed 3) (debug 0)))
  (let ((input-length (length input)))
    (when (zerop input-length)
      (return-from %unescape-as-uri ""))
    (let* ((input-index 0)
           (output (if (and may-modify-input-p
                            (adjustable-array-p input))
                       input
                       (make-array input-length :element-type 'character :adjustable t)))
           (output-index 0))
      (declare (type fixnum input-index output-index))
      (labels ((fail ()
                 (error "Parse error in unescape-as-uri for string ~S" input))
               (read-next-char (&optional must-exists-p)
                 (when (>= input-index input-length)
                   (if must-exists-p
                       (fail)
                       (return-from %unescape-as-uri (adjust-array output output-index))))
                 (prog1 (aref input input-index)
                   (incf input-index)))
               (write-next-char (char)
                 (setf (aref output output-index) char)
                 (incf output-index))
               (char-to-int (char)
                 (let ((result (digit-char-p char 16)))
                   (unless result
                     (fail))
                   result))
               (parse ()
                 (let ((next-char (read-next-char)))
                   (case next-char
                     (#\% (char%))
                     (#\+ (char+))
                     (t (write-next-char next-char)))
                   (parse)))
               (char% ()
                 (let ((next-char (read-next-char t)))
                   (write-next-char (code-char (case next-char
                                                 (#\u (+ (ash (char-to-int (read-next-char t)) 12)
                                                         (ash (char-to-int (read-next-char t)) 8)
                                                         (ash (char-to-int (read-next-char t)) 4)
                                                         (char-to-int (read-next-char t))))
                                                 (t (+ (ash (char-to-int next-char) 4)
                                                       (char-to-int (read-next-char t)))))))))
               (char+ ()
                 (write-next-char #\space)))
        (parse)))))

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

;; Copyright (c) 2002-2006, Edward Marco Baringer
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
