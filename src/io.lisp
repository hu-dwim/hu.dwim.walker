;; -*- lisp -*-

(in-package :it.bese.arnesi)

;;;; * Utilites for file system I/O

(defmacro with-input-from-file ((stream-name file-name &rest args) &body body)
  "Evaluate BODY with STREAM-NAME bound to an
  input-stream from file FILE-NAME. ARGS is passed
  directly to open."
  (when (member :direction args)
    (error "Can't specifiy :DIRECTION in WITH-INPUT-FILE."))
  `(with-open-file (,stream-name ,file-name :direction :input ,@args)
     ,@body))

(defmacro with-output-to-file ((stream-name file-name &rest args) &body body)
  "Evaluate BODY with STREAM-NAME to an output stream
  on the file named FILE-NAME. ARGS is sent as is to
  the call te open."
  (when (member :direction args)
    (error "Can't specifiy :DIRECTION in WITH-OUTPUT-FILE."))
  `(with-open-file (,stream-name ,file-name :direction :output ,@args)
     ,@body))

(defun read-string-from-file (pathname &key (buffer-size 4096)
                                            (element-type 'character))
  "Return the contents of PATHNAME as a string."
  (with-input-from-file (file-stream pathname)
    (with-output-to-string (datum) 
      (let ((buffer (make-array buffer-size :element-type element-type)))
	(loop for bytes-read = (read-sequence buffer file-stream)
	      do (write-sequence buffer datum :start 0 :end bytes-read)
	      while (= bytes-read buffer-size))))))

(defun write-string-to-file (string pathname &key (if-exists :error) (if-does-not-exist :error))
  "Write STRING to PATHNAME."
  (with-output-to-file (file-stream pathname :if-exists if-exists :if-does-not-exist if-does-not-exist)
    (write-sequence string file-stream)))

(defun copy-file (from to &key (if-to-exists :supersede)
                               (element-type '(unsigned-byte 8)))
  (with*
   (with-input-from-file (input  from :element-type element-type))
   (with-output-to-file  (output to   :element-type element-type
                                      :if-exists if-to-exists))
   (progn
     (loop
        with buffer-size = 4096
        with buffer = (make-array buffer-size :element-type element-type)
        for bytes-read = (read-sequence buffer input)
        while (= bytes-read buffer-size)
        do (write-sequence buffer output)
        finally (write-sequence buffer output :end bytes-read)))))

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
