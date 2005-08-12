;; -*- lisp -*-

(in-package :it.bese.arnesi)

;;;; * Manipulating strings

(defvar +lower-case-ascii-alphabet+
  "abcdefghijklmnopqrstuvwxyz"
  "All the lower case letters in 7 bit ASCII.")
(defvar +upper-case-ascii-alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "All the upper case letters in 7 bit ASCII.")
(defvar +ascii-alphabet+
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "All letters in 7 bit ASCII.")
(defvar +alphanumeric-ascii-alphabet+
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  "All the letters and numbers in 7 bit ASCII.")
(defvar +base64-alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  "All the characters allowed in base64 encoding.")

(defun random-string (&optional (length 32) (alphabet +ascii-alphabet+))
  "Returns a random alphabetic string.

The returned string will contain LENGTH characters chosen from
the vector ALPHABET.
"
  (loop with id = (make-string length)
        with alphabet-length = (length alphabet)
        for i below length
        do (setf (cl:aref id i)
                 (cl:aref alphabet (random alphabet-length)))
        finally (return id)))

(defun strcat (&rest items)
  "Returns a fresh string consisting of ITEMS concat'd together."
  (strcat* items))

(defun strcat* (string-designators)
  "Concatenate all the strings in STRING-DESIGNATORS."
  (with-output-to-string (strcat)
    (dotree (s string-designators)
      (princ s strcat))))

;;; A "faster" version for string concatenating.
;;; Could use just (apply #'concatenate 'string list), but that's quite slow
(defun join-strings (list)
  (let* ((length (reduce #'+ list :key #'length))
         (result (make-string length)))
    (loop
       for string in list
       for start = 0 then end
       for end = (+ start (length string))
       while string
       do (replace result string :start1 start :end1 end)
       finally (return result))))

(defun fold-strings (list)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((strings '())
        (result '()))
    (dolist (object list)
      (typecase object
        (string (push object strings))
        (t (when strings
             (push (join-strings (nreverse strings)) result)
             (setf strings '()))
           (push object result))))
    (when strings
      (push (join-strings (nreverse strings)) result))
    (nreverse result)))

(defun trim-string (string &optional (char '(#\Space #\Tab #\Newline
                                             #\Return #\Linefeed)))
  (let ((chars (ensure-list char)))
    (subseq string 
	    (loop for index upfrom 0 below (length string)
		  when (not (member (aref string index) chars)) 
		    do (return index)
		  ;; if we get here we're trimming the entire string
                  finally (return-from trim-string ""))
	    (loop for index downfrom (length string)
		  when (not (member (aref string (1- index)) chars))
		    do (return index)))))

(defvar ~%
  (format nil "~%")
  "A string containing a single newline")
(defvar ~T
  (string #\Tab)
  "A string containing a single tab character.")
(defvar +CR-LF+
  (make-array 2 :element-type 'character
                :initial-contents (list (code-char #x0D)
                                        (code-char #x0A)))
  "A string containing the two characters CR and LF.")
  
(defun ~D (number &optional stream &key mincol pad-char)
  (format stream "~v,vD" mincol pad-char number))

(defun ~A (object &optional stream)
  (format stream "~A" object))

(defun ~S (object &optional stream)
  (format stream "~S" object))

(defun ~W (object &optional stream)
  (format stream "~W" object))

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
