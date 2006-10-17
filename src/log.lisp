;; -*- lisp -*-

(in-package :it.bese.arnesi)

;;;; * A Trivial logging facility

;;;; A logger is a way to have the system generate a text message and
;;;; have that messaged saved somewhere for future review. Logging can
;;;; be used as a debugging mechanism or for just reporting on the
;;;; status of a system.

;;;; Logs are sent to a particular log category, each log category
;;;; sends the messages it receives to its handlers. A handler's job
;;;; is to take a message and write it somewhere. Log categories are
;;;; organized in a hierarchy and messages sent to a log category will
;;;; also be sent to that category's ancestors.

;;;; Each log category has a log level which is used to determine
;;;; whether are particular message should be processed or
;;;; not. Categories inherit their log level from their ancestors. If a
;;;; category has multiple fathers its log level is the min of the
;;;; levels of its fathers.

;;;; ** Log Levels

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +dribble+ 0)
  (defconstant +debug+   1)
  (defconstant +info+    2)
  (defconstant +warn+    3)
  (defconstant +error+   4)
  (defconstant +fatal+   5)

  (deflookup-table logger))

;;;; ** Log Categories

(defclass log-category ()
  ((ancestors :initform '()     :accessor ancestors :initarg :ancestors
              :documentation "The log categories this category inherits from.")
   (childer   :initform '()     :accessor childer   :initarg :childer
              :documentation "The log categories which inherit from this category.")
   (appenders :initform '()     :accessor appenders :initarg :appenders
              :documentation "A list of appender objects this category sholud send messages to.")
   (level     :initform +debug+ :initarg :level :accessor level
              :documentation "This category's log level.")
   (compile-time-level
              :initform +dribble+ :initarg :compile-time-level :accessor compile-time-level
              :documentation "This category's compile time log level. Any log expression below this level will macro-expand to NIL.")
   (name      :initarg :name :accessor name)))

(defmethod print-object ((category log-category) stream)
  (print-unreadable-object (category stream :type t :identity t)
    (if (slot-boundp category 'name)
        (format stream "~S" (name category))
        (format stream "#<NO NAME>"))))

(defmethod shared-initialize :after ((l log-category) slot-names
                                     &key ancestors &allow-other-keys)
  (declare (ignore slot-names))
  (dolist (anc ancestors)
    (pushnew l (childer anc) :test (lambda (a b)
				     (eql (name a) (name b))))))

;;; Runtime levels
(defmethod enabled-p ((cat log-category) level)
  (>= level (log.level cat)))

(defmethod log.level ((cat log-category))
  (with-slots (level) cat
    (or level
        (if (ancestors cat)
            (loop for ancestor in (ancestors cat)
                  minimize (log.level ancestor))
            (error "Can't determine level for ~S" cat)))))

(defmethod log.level ((cat-name symbol))
  (log.level (get-logger cat-name)))

(defmethod (setf log.level) (new-level (cat log-category)
                             &optional (recursive t))
  "Change the log level of CAT to NEW-LEVEL. If RECUSIVE is T the
  setting is also applied to the sub categories of CAT."
  (setf (slot-value cat 'level) new-level)
  (when recursive
    (dolist (child (childer cat))
      (setf (log.level child) new-level)))
  new-level)

(defmethod (setf log.level) (new-level (cat-name symbol) &optional (recursive t))
  (setf (log.level (get-logger cat-name) recursive) new-level))

(defmethod (setf log.level) (new-level (cat-name null) &optional (recursive t))
  (declare (ignore new-level cat-name recursive))
  (error "NIL does not specify a category."))

;;; Compile time levels
(defmethod compile-time-enabled-p ((cat log-category) level)
  (>= level (log.compile-time-level cat)))

(defmethod log.compile-time-level ((cat log-category))
  (with-slots (compile-time-level) cat
    (or compile-time-level
        (if (ancestors cat)
            (loop for ancestor in (ancestors cat)
                  minimize (log.compile-time-level ancestor))
            (error "Can't determine compile time level for ~S" cat)))))

(defmethod log.compile-time-level ((cat-name symbol))
  (log.compile-time-level (get-logger cat-name)))

(defmethod (setf log.compile-time-level) (new-level (cat log-category)
                                          &optional (recursive t))
  "Change the compile time log level of CAT to NEW-LEVEL. If RECUSIVE is T the
  setting is also applied to the sub categories of CAT."
  (setf (slot-value cat 'compile-time-level) new-level)
  (when recursive
    (dolist (child (childer cat))
      (setf (log.compile-time-level child) new-level)))
  new-level)

(defmethod (setf log.compile-time-level) (new-level (cat-name symbol) &optional (recursive t))
  (setf (log.compile-time-level (get-logger cat-name) recursive) new-level))

(defmethod (setf log.compile-time-level) (new-level (cat-name null) &optional (recursive t))
  (declare (ignore new-level cat-name recursive))
  (error "NIL does not specify a category."))

;;;; ** Handling Messages

(defmacro with-logging-io (&body body)
  `(let ((*print-right-margin* most-positive-fixnum)
         (*print-readably* nil))
    ,@body))

(defgeneric handle (category message level))

(defmethod handle :around ((cat log-category) message level)
  ;; turn off line wrapping for the entire time while inside the loggers
  (with-logging-io
    (call-next-method)))

(defmethod handle ((cat log-category) message level)
  (if (appenders cat)
      ;; if we have any appenders send them the message
      (dolist (appender (appenders cat))
	(append-message cat appender message level))
      ;; send the message to our ancestors
      (dolist (ancestor (ancestors cat))
	(handle ancestor message level))))

(defgeneric append-message (category log-appender message level))

;;;; *** Stream log appender

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass stream-log-appender ()
    ((stream :initarg :stream :accessor log-stream))
    (:documentation "Human readable to the console logger.")))

(defmethod make-instance ((class (eql (find-class 'stream-log-appender)))
                          &rest initargs)
  (declare (ignore initargs))
  (error "STREAM-LOG-APPENDER is an abstract class. You must use either brief-stream-log-appender or verbose-stream-log-appender objects."))

(defclass brief-stream-log-appender (stream-log-appender)
  ((last-message-year :initform 0)
   (last-message-month :initform 0)
   (last-message-day :initform 0))
  (:documentation "A subclass of stream-log-appender with minimal
  'overhead' text in log messages. This amounts to: not printing
  the package names of log categories and log levels and a more
  compact printing of the current time."))

(defclass verbose-stream-log-appender (stream-log-appender)
  ()
  (:documentation "A subclass of stream-log-appender which
  attempts to be as precise as possible, category names and log
  level names are printed with a package prefix and the time is
  printed in long format."))

(defmethod append-message :around ((category log-category) (s stream-log-appender)
                                   message level)
  (restart-case
      (call-next-method)
    (use-*debug-io* ()
      :report "Use the current value of *debug-io*"
      (setf (log-stream s) *debug-io*)
      (append-message category s message level))
    (use-*standard-output* ()
      :report "Use the current value of *standard-output*"
      (setf (log-stream s) *standard-output*)
      (append-message category s message level))
    (silence-logger ()
      :report "Ignore all future messages to this logger."
      (setf (log-stream s) (make-broadcast-stream)))))

(defmethod append-message ((category log-category) (s brief-stream-log-appender)
                           message level)
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time))
    (declare (ignore second))
    (with-slots (last-message-year last-message-month last-message-day)
        s
      (unless (and (= year last-message-year)
                   (= month last-message-month)
                   (= day last-message-day))
        (format (log-stream s) "--TIME MARK ~4,'0D-~2,'0D-~2,'0D--~%"
                year month day)
        (setf last-message-year year
              last-message-month month
              last-message-day day)))
    (format (log-stream s)
            "~2,'0D:~2,'0D ~A/~A: "
            hour minute
            (symbol-name (name category))
            (symbol-name level))
    (format (log-stream s) "~A~%" message)))

(defmethod append-message ((category log-category) (s verbose-stream-log-appender)
                            message level)
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time (get-universal-time))
    (format (log-stream s)
            "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D.~2,'0D ~S/~S: "
            year month date hour minute second
            (name category) level)
    (format (log-stream s) "~A~%" message)))

(defun make-stream-log-appender (&optional (stream *debug-io*))
  (make-instance 'verbose-stream-log-appender :stream stream))

(defclass file-log-appender (stream-log-appender)
  ((log-file :initarg :log-file :accessor log-file
             :documentation "Name of the file to write log messages to."))
  (:documentation "Logs to a file. the output of the file logger
  is not meant to be read directly by a human."))

(defmethod append-message ((category log-category) (appender file-log-appender)
                           message level)
  (with-output-to-file (log-file (log-file appender)
				 :if-exists :append
				 :if-does-not-exist :create)
    (let ((*package* #.(find-package :it.bese.arnesi)))
      (format log-file "(~S ~D ~S ~S)~%" level (get-universal-time) (name category) message))))

(defun make-file-log-appender (file-name)
  (make-instance 'file-log-appender :log-file file-name))

;;;; ** Creating Loggers

(defmacro deflogger (name ancestors &key compile-time-level level appender appenders documentation)
  (declare (ignore documentation)
           (type symbol name))
  (unless (eq (symbol-package name) *package*)
    (warn "When defining a logger named ~A the home package of the symbol is not *package* (not (eq ~A ~A)) "
          (let ((*package* (find-package "KEYWORD")))
            (format nil "~S" name))
          (symbol-package name) *package*))
  (when appender
    (setf appenders (append appenders (list appender))))
  (let ((ancestors (mapcar (lambda (ancestor-name)
			     `(or (get-logger ',ancestor-name)
				  (error "Attempt to define a sub logger of the undefined logger ~S."
					 ',ancestor-name)))
			   ancestors)))
    (flet ((make-log-helper (suffix level)
	     `(defmacro ,(intern (strcat name "." suffix)) (message-control &rest message-args)
                ;; first check at compile time
                (if (compile-time-enabled-p (get-logger ',name) ,level)
                    ;; then check at runtime
                    `(when (enabled-p (get-logger ',',name) ,',level)
                       ,(if message-args
                            `(handle (get-logger ',',name) (with-logging-io
                                                             (format nil ,message-control ,@message-args))
                                     ',',level)
                            `(handle (get-logger ',',name) ,message-control ',',level)))
                    (values)))))
      `(progn
         (eval-when (:load-toplevel :execute)
           (setf (get-logger ',name) (make-instance 'log-category
                                                    :name ',name
                                                    ,@(when level
                                                        `(:level ,level))
                                                    ,@(when compile-time-level
                                                        `(:compile-time-level ,compile-time-level))
                                                    :appenders (list ,@appenders)
                                                    :ancestors (list ,@ancestors))))
	 ,(make-log-helper '#:dribble '+dribble+)
	 ,(make-log-helper '#:debug '+debug+)
	 ,(make-log-helper '#:info '+info+)
	 ,(make-log-helper '#:warn '+warn+)
	 ,(make-log-helper '#:error '+error+)
	 ,(make-log-helper '#:fatal '+fatal+)
        (values)))))



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
