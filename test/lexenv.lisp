;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.walker/test)

(defmacro %compile-quoted (form)
  `(compile nil '(lambda () ,form)))

(def macro run-in-lexical-environment ((env-variable form &key (compiler '%compile-quoted)) &body forms)
  "Executes FORMS with lexical environment captured at the point marked with the symbol -HERE-."
  ;; Use private interned symbols to ensure that the body can be printed readably:
  (with-unique-names (body injector-macro)
    `(let ((,body (lambda (,env-variable)
                    ;; TODO: wrap the body in our own handlers that will prevent the errors/failed-asserts reaching COMPILE
                    ,@forms)))
       (declare (special ,body))        ; For the macrolet
       (handler-bind
           (#+sbcl(sb-ext:compiler-note #'muffle-warning)
            (warning #'muffle-warning))
         (,compiler
          ,(let* ((inject-form `(macrolet ((,injector-macro (&environment env)
                                             (declare (special ,body))
                                             (funcall ,body env)
                                             (values)))
                                  (,injector-macro)))
                  (replaced (subst inject-form '-here- form)))
             (assert (not (equal form replaced)) nil
                     "~S: couldn't find ~S in form ~S"
                     -this-definition/name- '-here form)
             replaced)))
       (values))))

(def suite* (test/lexenv :in test))

(def suite* (test/lexenv/query :in test/lexenv))

;; TODO harmonize its name with RUN-IN-LEXICAL-ENVIRONMENT
(def macro with-captured-compile-environment ((env-var &rest symbols) form-generator &body code)
  (let ((package-id (package-name *package*)))
    (with-unique-names (temp-lisp-filename temp-fasl-filename compiler warn? error?)
      `(let* ((*package* (find-package ',package-id))
              ,@(mapcar (lambda (symbol)
                          `(,symbol (intern (format nil "SPEC-SYM-~A-~A" ',symbol (random 10000))
                                            ',package-id)))
                        symbols)
              (,temp-lisp-filename #+unix "/tmp/TMP-WALKER-CE-TEST.lisp"
                                   #-unix "TMP-WALKER-CE-TEST.lisp")
              (,temp-fasl-filename NIL)
              ,warn? ,error?)
         (unwind-protect
              (flet ((,compiler (form)
                       (with-output-to-file (*standard-output* ,temp-lisp-filename)
                         (bind ((*print-readably* t))
                           (write '(in-package ,package-id))
                           (write form)
                           (write '(eval-when (:load-toplevel :execute)
                                     (error "This was not meant to be loaded!")))))
                       (multiple-value-setq (,temp-fasl-filename ,warn? ,error?)
                         (compile-file ,temp-lisp-filename))
                       (is (not (or (null,temp-fasl-filename) ,warn? ,error?))
                           "Temporary file compilation failed.")))
                (run-in-lexical-environment (,env-var ,form-generator :compiler ,compiler)
                  ,@code))
           ,@(mapcar (lambda (symbol) `(unintern ,symbol)) symbols)
           (delete-file ,temp-lisp-filename)
           (when ,temp-fasl-filename
             (delete-file ,temp-fasl-filename)))))))

(def test test/lexenv/query/variables ()
  (run-in-lexical-environment
      (env (symbol-macrolet ((a 42)
                             (b 43))
             (flet ((f1 () 1)
                    (f2 () 2))
               (macrolet ((m1 () 1)
                          (m2 () 2))
                 (let* ((x 1)
                        (y 2)
                        (z 3))
                   (declare (ignore z))
                   -here-)))))
    (is (subsetp '(y x) (collect-variables-in-lexenv env)))
    (bind ((ignored 0)
           (non-ignored 0))
      (do-variables-in-lexenv (env name :ignored? ignored?)
        (when (eq (symbol-package name) *package*)
          (is (symbolp name))
          (if ignored?
              (incf ignored)
              (incf non-ignored))))
      (is (= ignored 1))
      (is (= non-ignored 2)))
    (is (subsetp '(z y x)
               (collect-variables-in-lexenv env :include-ignored? t)))
    (is (find-variable-in-lexenv 'x env))
    (is (not (find-variable-in-lexenv 'z env)))
    (is (not (find-variable-in-lexenv 'a env)))
    (is (not (find-variable-in-lexenv 'f1 env)))
    (is (not (find-variable-in-lexenv 'm1 env)))
    (is (find-variable-in-lexenv 'z env :include-ignored? t))))

(def test test/lexenv/query/functions ()
  (run-in-lexical-environment
      (env (symbol-macrolet ((a 42)
                             (b 43))
             (flet ((f1 () 1)
                    (f2 () 2))
               (macrolet ((m1 () 1)
                          (m2 () 2))
                 (let ((x 1)
                       (y 2)
                       (z 3))
                   (declare (ignore z))
                   -here-)))))
    (is (equal '(f2 f1)
               (collect-functions-in-lexenv env)))
    (bind ((functions 0))
      (do-functions-in-lexenv (env name)
        (is (and (symbolp name)
                 (eq (symbol-package name) *package*)))
        (incf functions))
      (is (= functions 2)))
    (is (find-function-in-lexenv 'f1 env))
    (is (not (find-function-in-lexenv 'foo env)))
    (is (not (find-function-in-lexenv 'a env)))
    (is (not (find-function-in-lexenv 'm1 env)))
    (is (not (find-function-in-lexenv '-here- env)))))

(def test test/lexenv/query/macros ()
  (run-in-lexical-environment
      (env (symbol-macrolet ((a 42)
                             (b 43))
             (flet ((f1 () 1)
                    (f2 () 2))
               (macrolet ((m1 () 1)
                          (m2 () 2))
                 (let ((x 1)
                       (y 2)
                       (z 3))
                   (declare (ignore z))
                   -here-)))))
    (is (subsetp '(m1 m2) (collect-macros-in-lexenv env)))
    (bind ((macros 0))
      (do-macros-in-lexenv (env name fn)
        (when (eq (symbol-package name) *package*)
          (is (symbolp name))
          (is (functionp fn))
          (incf macros)))
      (is (= macros 2)))
    (is (find-macro-in-lexenv 'm1 env))
    (is (not (find-macro-in-lexenv 'f1 env)))
    (is (not (find-macro-in-lexenv 'a env)))
    (is (not (find-macro-in-lexenv 'x env)))))

(deftest test/lexenv/query/symbol-macros ()
  (run-in-lexical-environment
      (env (symbol-macrolet ((a 42)
                             (b 43))
             (flet ((f1 () 1)
                    (f2 () 2))
               (macrolet ((m1 () 1)
                          (m2 () 2))
                 (let ((x 1)
                       (y 2)
                       (z 3))
                   (declare (ignore z))
                   -here-)))))
    (is (set-equal (collect-symbol-macros-in-lexenv env)
                   '(a b)))
    (bind ((symbol-macros 0))
      (do-symbol-macros-in-lexenv (env name definition)
        (is (and (symbolp name)
                 (eq (symbol-package name) *package*)))
        (is (not (functionp definition)))
        (incf symbol-macros))
      (is (= symbol-macros 2)))
    (is (find-symbol-macro-in-lexenv 'a env))
    (is (not (find-symbol-macro-in-lexenv 'm1 env)))
    (is (not (find-symbol-macro-in-lexenv 'f1 env)))
    (is (not (find-symbol-macro-in-lexenv 'x env)))))

#-(or openmcl allegro)
(deftest test/lexenv/query/blocks ()
  (run-in-lexical-environment
      (env (block b1
             (flet ((f1 () 1)
                    (f2 () 2))
               (block b2
                 (let ((x 1)
                       (y 2)
                       (z 3))
                   (declare (ignore z))
                   -here-)))))
    (is (equal '(b2 b1)
               (remove-if-not #'symbol-package
                              (collect-blocks-in-lexenv env))))
    (bind ((blocks 0))
      (do-blocks-in-lexenv (env name)
        (when (symbol-package name)
          (is (and (symbolp name)
                   (eq (symbol-package name) *package*)))
          (incf blocks)))
      (is (= blocks 2)))
    (is (find-block-in-lexenv 'b1 env))
    (is (not (find-block-in-lexenv '-here- env)))
    (is (not (find-block-in-lexenv 'f1 env)))
    (is (not (find-block-in-lexenv 'x env)))))

#-(or openmcl allegro)
(deftest test/lexenv/query/tags ()
  (run-in-lexical-environment
      (env (block b1
             (tagbody
              t1
                (progn)
              t2
                (tagbody
                 t21
                   (progn)
                 t22
                   (block b2
                     -here-)))))
    (is (set-equal (collect-tags-in-lexenv env)
                   '(t21 t22 t1 t2)))
    (bind ((tags 0))
      (do-tags-in-lexenv (env name)
        (is (and (symbolp name)
                 (eq (symbol-package name) *package*)))
        (incf tags))
      (is (= tags 4)))
    (is (find-tag-in-lexenv 't1 env))
    (is (not (find-tag-in-lexenv '-here- env)))
    (is (not (find-tag-in-lexenv 'f1 env)))
    (is (not (find-tag-in-lexenv 'x env)))))

(def test test/lexenv/query/special-variable ()
  (run-in-lexical-environment
      (env (let ((x 42))
             (declare (special x))
             -here-))
    (is (special-variable-name? 'x env))))

(def test test/lexenv/query/compiled-special-variable ()
  (with-captured-compile-environment (env varname)
      `(progn
         (declaim (type fixnum ,varname))
         (defvar ,varname)
         -here-)
    (is (proclaimed-special-variable?/global varname env))
    (is (special-variable-name? varname env))
    (is (equal (declared-variable-type/global varname) 'fixnum))))

;; TODO (defsuite* (test/lexenv/augment :in test/lexenv))
