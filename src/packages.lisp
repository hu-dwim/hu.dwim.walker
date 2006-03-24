;; -*- lisp -*-

(in-package :common-lisp-user)

;;;; * Introduction

;;;; It is a collection of lots of small bits and pieces which have
;;;; proven themselves usefull in various applications. They are all
;;;; tested, some even have a test suite and a few are even
;;;; documentated.

(defpackage :it.bese.arnesi
  (:documentation "The arnesi utility suite.")
  (:nicknames :arnesi)
  (:use :common-lisp)
  (:export

   #:clean-op

   #:make-reducer
   #:make-pusher
   #:make-collector
   #:with-reducer
   #:with-collector
   #:with-collectors

   #:walk-form
   #:make-walk-env
   #:*walk-handlers*
   #:defwalker-handler
   #:implicit-progn-mixin
   #:implicit-progn-with-declare-mixin
   #:binding-form-mixin
   #:declaration-form
   #:constant-form
   #:variable-reference
   #:local-variable-reference
   #:local-lexical-variable-reference
   #:free-variable-reference
   #:application-form
   #:local-application-form
   #:lexical-application-form
   #:free-application-form
   #:lambda-application-form
   #:function-form
   #:lambda-function-form
   #:function-object-form
   #:local-function-object-form
   #:free-function-object-form
   #:lexical-function-object-form
   #:function-argument-form
   #:required-function-argument-form
   #:specialized-function-argument-form
   #:optional-function-argument-form
   #:keyword-function-argument-form
   #:allow-other-keys-function-argument-form
   #:rest-function-argument-form
   #:block-form
   #:return-from-form
   #:catch-form
   #:throw-form
   #:eval-when-form
   #:if-form
   #:function-binding-form
   #:flet-form
   #:labels-form
   #:variable-binding-form
   #:let-form
   #:let*-form
   #:locally-form
   #:macrolet-form
   #:multiple-value-call-form
   #:multiple-value-prog1-form
   #:progn-form
   #:progv-form
   #:setq-form
   #:symbol-macrolet-form
   #:tagbody-form
   #:go-tag-form
   #:go-form
   #:the-form
   #:extract-argument-names
   #:walk-lambda-list

   #:unwalk-form

   #:to-cps
   #:with-call/cc
   #:kall
   #:call/cc
   #:let/cc
   #:*call/cc-returns*
   #:invalid-return-from
   #:unreachable-code
   #:defun/cc
   #:defgeneric/cc
   #:defmethod/cc
   #:fmakun-cc
   #:*debug-evaluate/cc*
   #:*trace-cc*
   
   #:ppm
   #:ppm1
   #:apropos-list*
   #:apropos*

   #:with-input-from-file
   #:with-output-to-file
   #:read-string-from-file
   #:write-string-to-file
   #:copy-file
   #:copy-stream
   #:string-to-octets
   #:octets-to-string
   
   #:if-bind
   #:aif
   #:when-bind
   #:awhen
   #:cond-bind
   #:acond
   #:aand
   #:and-bind
   #:it
   #:whichever
   #:xor
   #:switch
   #:eswitch
   #:cswitch

   #:build-hash-table
   #:deflookup-table
   #:hash-to-alist
   #:hash-table-keys
   #:hash-table-values
   
   #:write-as-uri
   #:escape-as-uri
   #:unescape-as-uri
   #:nunescape-as-uri
   #:write-as-html
   #:escape-as-html
   #:unescape-as-html
   
   #:compose
   #:conjoin
   #:curry
   #:rcurry
   #:noop
   #:y
   #:lambda-rec

   #:dolist*
   #:dotree
   #:ensure-list
   #:ensure-cons
   #:partition
   #:partitionx
   #:proper-list-p
   #:push*

   #:get-logger
   #:log-category
   #:stream-log-appender
   #:brief-stream-log-appender
   #:verbose-stream-log-appender
   #:make-stream-log-appender
   #:file-log-appender
   #:make-file-log-appender
   #:log.dribble
   #:log.debug
   #:log.info
   #:log.warn
   #:log.error
   #:log.fatal
   #:deflogger
   #:log.level
   #:+dribble+
   #:+debug+
   #:+info+
   #:+warn+
   #:+error+
   #:+fatal+
   #:handle
   #:append-message
   #:ancestors
   #:appenders
   #:childer
   
   #:with-unique-names
   #:rebinding
   #:define-constant

   #:make-matcher
   #:match
   #:match-case
   #:list-match-case
   
   #:parse-ieee-double
   #:parse-float
   #:mulf
   #:divf
   #:minf
   #:maxf
   #:map-range
   #:do-range
   #:10^
   
   #:tail
   #:but-tail
   #:head
   #:but-head
   #:starts-with
   #:ends-with
   #:read-sequence*
   #:deletef
   
   #:+lower-case-ascii-alphabet+
   #:+upper-case-ascii-alphabet+
   #:+ascii-alphabet+
   #:+alphanumeric-ascii-alphabet+
   #:+base64-alphabet+
   #:random-string
   #:strcat
   #:strcat*
   #:princ-csv
   #:parse-csv-string
   #:join-strings
   #:fold-strings
   #:~%
   #:~T
   #:+CR-LF+
   #:~D
   #:~A
   #:~S
   #:~W

   #:def-special-environment
   
   #:intern-concat

   #:vector-push-extend*
   #:string-from-array

   #:queue
   #:enqueue
   #:dequeue
   #:peek-queue
   #:queue-empty-p
   #:queue-count
   #:lru-queue
   
   ;; decimal arith
   #:*precision*
   #:with-precision
   #:decimal-from-float
   #:float-from-decimal
   #:round-down
   #:round-half-up
   #:round-half-even
   #:round-ceiling
   #:round-floor
   #:round-half-down
   #:round-up

   #:enable-sharp-l

   #:enable-bracket-reader
   #:with-package

   #:defclass-struct

   #:with*

   #:quit

   #:wrapping-standard

   #:levenshtein-distance

   ))

;;;; * Colophon

;;;; This documentation was produced by qbook.

;;;; arnesi, and the associated documentation, is written by Edward
;;;; Marco Baringer <mb@bese.it>.

;;;; ** COPYRIGHT

;;;; Copyright (c) 2002-2006, Edward Marco Baringer
;;;; All rights reserved. 

;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions are
;;;; met:

;;;;  - Redistributions of source code must retain the above copyright
;;;;    notice, this list of conditions and the following disclaimer.

;;;;  - Redistributions in binary form must reproduce the above copyright
;;;;    notice, this list of conditions and the following disclaimer in the
;;;;    documentation and/or other materials provided with the distribution.

;;;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;;;    of its contributors may be used to endorse or promote products
;;;;    derived from this software without specific prior written permission.

;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;;;@include "accumulation.lisp"

;;;;@include "asdf.lisp"

;;;;@include "compat.lisp"

;;;; / @include "cps.lisp"

;;;;@include "csv.lisp"

;;;;@include "debug.lisp"

;;;;@include "decimal-arithmetic.lisp"

;;;;@include "defclass-struct.lisp"

;;;;@include "flow-control.lisp"

;;;;@include "hash.lisp"

;;;;@include "http.lisp"

;;;;@include "io.lisp"

;;;;@include "lambda.lisp"

;;;;@include "list.lisp"

;;;;@include "log.lisp"

;;;;@include "matcher.lisp"

;;;;@include "mop.lisp"

;;;;@include "mopp.lisp"

;;;;@include "numbers.lisp"

;;;;@include "one-liners.lisp"

;;;;@include "sequence.lisp"

;;;;@include "sharpl-reader.lisp"

;;;;@include "specials.lisp"

;;;;@include "string.lisp"

;;;;@include "walk.lisp"

;;;;@include "vector.lisp"

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
