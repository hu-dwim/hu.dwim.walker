;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.walker)

;;;; Atoms

(def (class* e) constant-form (walked-form)
  ((value)))

(def unwalker constant-form (value)
  (if (or (eq value t)
          (eq value nil)
          (keywordp value))
      value
      (typecase value
        (symbol `(quote ,value))
        (cons   `(quote ,value))
        (t value))))

(def print-object constant-form
  (format t "!~S" (value-of -self-)))

(def (class* e) variable-reference-form (named-walked-form)
  ())

(def unwalker variable-reference-form (name)
  name)

(def print-object variable-reference-form
  (format t "!~S" (name-of -self-)))

(def (class* e) lexical-variable-reference-form (variable-reference-form)
  ())

(def (class* e) walked-lexical-variable-reference-form (lexical-variable-reference-form)
  ((definition))
  (:documentation "A reference to a local variable defined in the lexical environment inside the form passed to walk-form."))

(def (class* e) unwalked-lexical-variable-reference-form (lexical-variable-reference-form)
  ()
  (:documentation "A reference to a local variable defined in the lexical environment outside of the form passed to walk-form."))

;; TODO should we add/handle walked-special-variable-reference-form?
(def (class* e) special-variable-reference-form (variable-reference-form)
  ())

(def (class* e) free-variable-reference-form (special-variable-reference-form)
  ())

(def walker +atom-marker+
  (bind ((lexenv (env/lexical-environment -environment-)))
    (cond
      ((constant-name? -form-)
       (make-form-object 'constant-form -parent- :value -form-))
      (t
       (bind (((:values closest-lexenv-entry-type nil definition) (-lookup- '(:variable :unwalked-variable :symbol-macro) -form-)))
         (cond
           (closest-lexenv-entry-type
            ;; we check the closest entry in the lexenv with one of the listed types (i.e. skipping type DECLARE's). please note that it is NOT THE SAME AS
            ;; looking for each type individually in three separate calls to LOOKUP!
            (ecase closest-lexenv-entry-type
              (:variable
               (make-form-object 'walked-lexical-variable-reference-form -parent- :name -form- :definition definition))
              (:unwalked-variable
               (make-form-object 'unwalked-lexical-variable-reference-form -parent- :name -form-))
              (:symbol-macro
               (bind ((*inside-macroexpansion* t))
                 (recurse (-lookup- :symbol-macro -form-))))))
           ((symbol-macro-name? -form- lexenv)
            (recurse (walker-macroexpand-1 -form- lexenv)))
           ;; FIXME special variable handling is most probably not good as it is:
           ;; check proper behavior regarding the lexenv nesting and the parent walking below for (DECLARE (SPECIAL ...)) entries
           ((or (special-variable-name? -form- lexenv)
                (loop
                   :for node = -parent- :then (parent-of node)
                   :while node
                   :do (progn
                         (when (and (typep node 'implicit-progn-with-declarations-mixin)
                                    (progn
                                      (find-if (lambda (declare)
                                                 (and (typep declare 'special-variable-declaration-form)
                                                      (eq (name-of declare) -form-)))
                                               (declarations-of node))))
                           (return t)))))
            (make-form-object 'special-variable-reference-form -parent- :name -form-))
           (t
            (handle-undefined-reference :variable -form-)
            (make-form-object 'free-variable-reference-form -parent- :name -form-))))))))

;;;; BLOCK/RETURN-FROM

(def (class* e) block-form (named-walked-form implicit-progn-mixin)
  ())

(def walker block
  (bind (((block-name &rest body) (cdr -form-)))
    (with-form-object (block 'block-form -parent-
                             :name block-name)
      (walk-implict-progn block
                          body
                          (-augment- :block block-name block)))))

(def unwalker block-form (name body)
  `(block ,name ,@(recurse-on-body body)))

(def (class* e) return-from-form (walked-form)
  ((target-block nil)
   (result)))

(def (condition* e) return-from-unknown-block (walker-error)
  ((block-name))
  (:report (lambda (condition stream)
             (format stream "Unable to return from block named ~S." (block-name-of condition)))))

(def walker return-from
  (bind (((block-name &optional (value '(values))) (rest -form-)))
    (if (-lookup- :block block-name)
        (with-form-object (return-from 'return-from-form -parent-
                                       :target-block (-lookup- :block block-name))
          (setf (result-of return-from) (recurse value return-from)))
        (restart-case
            (error 'return-from-unknown-block :block-name block-name)
          (add-block ()
            :report "Add this block and continue."
            (recurse -form- -parent- (-augment- :block block-name :unknown-block)))))))

(def unwalker return-from-form (target-block result)
  (bind ((unwalked-result (recurse result))
         (result-form (if (equal unwalked-result '(values))
                          '()
                          (list unwalked-result))))
    (if (null (name-of target-block))
        `(return ,@result-form)
        `(return-from ,(name-of target-block) ,@result-form))))

;;;; CATCH/THROW

(def (class* e) catch-form (walked-form implicit-progn-mixin)
  ((tag)))

(def walker catch
  (bind (((tag &body body) (cdr -form-)))
    (with-form-object (catch 'catch-form -parent-)
      (setf (tag-of catch) (recurse tag catch))
      (walk-implict-progn catch body -environment-))))

(def unwalker catch-form (tag body)
  `(catch ,(recurse tag) ,@(recurse-on-body body)))

(def (class* e) throw-form (walked-form)
  ((tag)
   (value)))

(def walker throw
  (bind (((tag &optional (result '(values))) (cdr -form-)))
    (with-form-object (throw 'throw-form -parent-)
      (setf (tag-of throw) (recurse tag throw)
            (value-of throw) (recurse result throw)))))

(def unwalker throw-form (tag value)
  `(throw ,(recurse tag) ,(recurse value)))

;;;; EVAL-WHEN

(def (class* e) eval-when-form (walked-form implicit-progn-mixin)
  ((eval-when-times)))

(def walker eval-when
  (bind (((times &body body) (cdr -form-)))
    (with-form-object (eval-when 'eval-when-form -parent-)
      (setf (eval-when-times-of eval-when) times)
      (walk-implict-progn eval-when body -environment- :declarations-allowed nil))))

(def unwalker eval-when-form (body eval-when-times)
  `(eval-when ,eval-when-times
     ,@(recurse-on-body body)))

;;;; IF

(def (class* e) if-form (walked-form)
  ((condition)
   (then)
   (else)))

(def walker if
  (with-form-object (if 'if-form -parent-)
    (setf (condition-of if) (recurse (second -form-) if)
          (then-of if) (recurse (third -form-) if)
          (else-of if) (recurse (fourth -form-) if))))

(def unwalker if-form (condition then else)
  `(if ,(recurse condition)
       ,(recurse then)
       ,@(awhen (recurse else)
           (list it))))

;;;; LET/LET*

(def (class* ea) lexical-variable-binder-form (walked-form
                                               binder-form-mixin
                                               implicit-progn-with-declarations-mixin)
  ())

(def (class* ea) lexical-variable-binding-form (named-walked-form)
  ((initial-value)))

(def (class* ea) let-form (lexical-variable-binder-form)
  ())

(def walker let
  (with-form-object (let 'let-form -parent-)
    (setf (bindings-of let)
          (mapcar (lambda (binding)
                    (with-current-form binding
                      (bind (((name &optional initial-value) (ensure-list (coerce-to-form binding))))
                        (with-form-object (binding 'lexical-variable-binding-form -parent- :name name)
                          (setf (initial-value-of binding) (recurse initial-value binding))))))
                  (coerce-to-form (second -form-))))
    (walk-implict-progn
     let (cddr -form-) -environment-
     :declarations-allowed t
     :declarations-callback (lambda (declarations)
                              ;; extend the walkenv before we walk the body with the lexical variables introduced by this LET form
                              (loop
                                :for binding :in (bindings-of let)
                                :for name = (name-of binding)
                                :for lexenv = (env/lexical-environment -environment-)
                                :do (when (and (not (special-variable-name? name lexenv))
                                               (not (find-if (lambda (declaration)
                                                               (and (typep declaration 'special-variable-declaration-form)
                                                                    (eq name (name-of declaration))))
                                                             declarations)))
                                      (-augment- :variable name binding)))
                              ;; we've extended the env, inform WALK-IMPLICT-PROGN about it
                              -environment-))))

(def function let/let*-form-unwalker (name bindings body declarations)
  `(,name ,(mapcar (lambda (bind)
                     (list (name-of bind) (unwalk-form (initial-value-of bind))))
                   bindings)
     ,@(unwalk-declarations declarations)
     ,@(unwalk-forms body)))

(def unwalker let-form (bindings body declarations)
  (let/let*-form-unwalker 'let bindings body declarations))

(def (class* e) let*-form (lexical-variable-binder-form)
  ())

(def walker let*
  (with-form-object (let*-form 'let*-form -parent- :bindings '())
    (walk-implict-progn
     let*-form (cddr -form-) -environment-
     :declarations-allowed t
     :declarations-callback (lambda (declarations)
                              ;; extend the walkenv before we walk the body with the lexical variables introduced by this LET* form
                              (setf (bindings-of let*-form)
                                    (loop
                                      :for entry :in (second -form-)
                                      :for lexenv = (env/lexical-environment -environment-)
                                      :collect (bind (((name &optional initial-value) (ensure-list entry)))
                                                 (with-form-object (binding 'lexical-variable-binding-form let*-form :name name)
                                                   (setf (initial-value-of binding) (recurse initial-value binding))
                                                   (when (and (not (special-variable-name? name lexenv))
                                                              (not (find-if (lambda (declaration)
                                                                              (and (typep declaration 'special-variable-declaration-form)
                                                                                   (eq name (name-of declaration))))
                                                                            declarations)))
                                                     (-augment- :variable name binding))))))
                              ;; we've extended the env, inform WALK-IMPLICT-PROGN about it
                              -environment-))))

(def unwalker let*-form (bindings body declarations)
  (let/let*-form-unwalker 'let* bindings body declarations))

;;;; LOCALLY

(def (class* e) locally-form (walked-form
                              implicit-progn-with-declarations-mixin)
  ())

(def walker locally
  (with-form-object (locally 'locally-form -parent-)
    (walk-implict-progn locally (cdr -form-) -environment- :declarations-allowed t)))

(def unwalker locally-form (body declarations)
  `(locally
       ,@(unwalk-declarations declarations)
     ,@(recurse-on-body body)))

;;;; MACROLET

(def (class* e) macrolet-form (walked-form
                               binder-form-mixin
                               implicit-progn-with-declarations-mixin)
  ())

(def walker macrolet
  ;; TODO is there any point in constructing a macrolet form if we macroexpand the body anyways?
  (with-form-object (macrolet 'macrolet-form -parent-
                              :bindings '())
    (dolist* ((name args &body body) (second -form-))
      (bind ((handler (parse-macro-definition name args body (env/lexical-environment -environment-))))
        (-augment- :macro name handler)
        ;; there's not much point in keeping the bindings when we expand the macrolet body anyway, so don't.
        ;; it would just hinder the saving of the form into fasl's for no apparent benefit.
        (push (cons name nil) (bindings-of macrolet))))
    (setf (bindings-of macrolet) (nreverse (bindings-of macrolet)))
    (walk-implict-progn macrolet (cddr -form-) -environment- :declarations-allowed t)))

(def unwalker macrolet-form (body declarations)
  ;; We ignore the bindings, because the expansion has already taken place at walk-time.
  ;; TODO either walk into a locally, or unwalk the bindings here
  `(locally ,@(unwalk-declarations declarations) ,@(recurse-on-body body)))

;;;; MULTIPLE-VALUE-CALL

(def (class* e) multiple-value-call-form (walked-form)
  ((function-designator)
   (arguments)))

(def walker multiple-value-call
  (with-form-object (m-v-c 'multiple-value-call-form -parent-)
    (setf (function-designator-of m-v-c) (recurse (second -form-) m-v-c)
          (arguments-of m-v-c) (mapcar (lambda (f)
                                         (recurse f m-v-c))
                                       (cddr -form-)))))

(def unwalker multiple-value-call-form (function-designator arguments)
  `(multiple-value-call ,(recurse function-designator) ,@(recurse-on-body arguments)))

;;;; MULTIPLE-VALUE-PROG1

(def (class* e) multiple-value-prog1-form (walked-form)
  ((first-form)
   (other-forms)))

(def walker multiple-value-prog1
  (with-form-object (m-v-p1 'multiple-value-prog1-form -parent-)
    (setf (first-form-of m-v-p1) (recurse (second -form-) m-v-p1)
          (other-forms-of m-v-p1) (mapcar (lambda (f)
                                            (recurse f m-v-p1))
                                       (cddr -form-)))))

(def unwalker multiple-value-prog1-form (first-form other-forms)
  `(multiple-value-prog1 ,(recurse first-form) ,@(recurse-on-body other-forms)))

;;;; PROGN

(def (class* e) progn-form (walked-form implicit-progn-mixin)
  ())

(def walker progn
  (with-form-object (progn 'progn-form -parent-)
    (walk-implict-progn progn (cdr -form-) -environment-)))

(def unwalker progn-form (body)
  `(progn ,@(recurse-on-body body)))

(def print-object progn-form
  (bind ((body (body-of -self-)))
    (pprint-logical-block (*standard-output* body :prefix "(" :suffix ")")
      (princ "progn")
      (pprint-indent :block 1)
      (pprint-newline :mandatory)
      (pprint-exit-if-list-exhausted)
      (loop
         :with first? = t
         :for el = (pprint-pop)
         :do
         (unless first?
           (pprint-newline :mandatory))
         (princ el)
         (pprint-exit-if-list-exhausted)
         (setf first? nil)))))

;;;; PROGV

(def (class* e) progv-form (walked-form implicit-progn-mixin)
  ((variables-form)
   (values-form)))

(def walker progv
  (with-form-object (progv 'progv-form -parent-)
    (setf (variables-form-of progv) (recurse (cadr -form-) progv))
    (setf (values-form-of progv) (recurse (caddr -form-) progv))
    (walk-implict-progn progv (cdddr -form-) -environment-)
    progv))

(def unwalker progv-form (body variables-form values-form)
  `(progv
       ,(recurse variables-form)
       ,(recurse values-form)
     ,@(recurse-on-body body)))

;;;; QUOTE

(def walker quote
  (make-form-object 'constant-form -parent-
                    :value (second -form-)))

;;;; SETQ

(def (class* e) setq-form (walked-form)
  ((variable)
   (value)))

(def walker setq
  ;; the SETQ handler needs to be able to deal with symbol-macrolets
  ;; which haven't yet been expanded and may expand into something
  ;; requiring setf and not setq.
  (let ((effective-code '()))
    (loop
       :for (name value) :on (cdr -form-) :by #'cddr
       :do (push (aif (-lookup- :symbol-macro name)
                      `(setf ,it ,value)
                      `(setq ,name ,value))
                 effective-code))
    (if (= 1 (length effective-code))
        ;; only one form, the "simple case"
        (bind (((type variable value) (first effective-code)))
          (ecase type
            (setq (with-form-object (setq 'setq-form -parent-)
                    (setf (variable-of setq) (recurse variable setq))
                    (setf (value-of setq) (recurse value setq))))
            (setf (recurse (first effective-code)))))
        ;; multiple forms
        (with-form-object (progn 'progn-form -parent-)
          (walk-implict-progn progn effective-code -environment-)))))

(def unwalker setq-form (variable value)
  `(setq ,(recurse variable) ,(recurse value)))

;;;; SYMBOL-MACROLET

(def (class* e) symbol-macrolet-form (walked-form
                                      binder-form-mixin
                                      implicit-progn-with-declarations-mixin)
  ())

(def walker symbol-macrolet
  (with-form-object (symbol-macrolet 'symbol-macrolet-form -parent-
                                     :bindings '())
    (dolist* ((symbol expansion) (second -form-))
      (-augment- :symbol-macro symbol expansion)
      (push (cons symbol expansion) (bindings-of symbol-macrolet)))
    (nreversef (bindings-of symbol-macrolet))
    (walk-implict-progn symbol-macrolet (cddr -form-) -environment- :declarations-allowed t)))

(def unwalker symbol-macrolet-form (body declarations)
  ;; We ignore the bindings, because the expansion has already taken place at walk-time.
  ;; TODO either walk into a locally, or unwalk the bindings here
  `(locally ,@(unwalk-declarations declarations) ,@(recurse-on-body body)))

;;;; TAGBODY/GO

(def (class* e) tagbody-form (walked-form implicit-progn-mixin)
  ())

(def walker tagbody
  (with-form-object (tagbody 'tagbody-form -parent-
                             :body (cdr -form-))
    (-augment- :tagbody 'enclosing-tagbody tagbody)
    (flet ((go-tag? (form)
             (or (symbolp form)
                 (integerp form))))
      ;; the loop below destructuivly modifies the body of tagbody,
      ;; since it's the same object as the source we need to copy it.
      (setf (body-of tagbody) (copy-list (body-of tagbody)))
      (loop
         :for part :on (body-of tagbody)
         :if (go-tag? (car part))
           :do (-augment- :tag (car part) (cdr part)))
      (loop
         :for part :on (body-of tagbody)
         :if (go-tag? (car part))
           :do (setf (car part) (with-current-form (car part)
                                  (make-form-object 'go-tag-form tagbody
                                                    :name (car part))))
         :else
           :do (setf (car part) (recurse (car part) tagbody))))))

(def unwalker tagbody-form (body)
  `(tagbody ,@(recurse-on-body body)))

(def (class* e) go-tag-form (named-walked-form)
  ())

(def unwalker go-tag-form (name)
  name)

(def (class* e) go-form (named-walked-form)
  ((jump-target)
   (enclosing-tagbody)))

(def walker go
  (make-form-object 'go-form -parent-
                    :name (second -form-)
                    :jump-target (-lookup- :tag (second -form-))
                    :enclosing-tagbody (-lookup- :tagbody 'enclosing-tagbody)))

(def unwalker go-form (name)
  `(go ,name))

;;;; THE

(def (class* e) the-form (walked-form)
  ((type)
   (value)))

(def walker the
  (with-form-object (the 'the-form -parent-
                         :type (second -form-))
    (setf (value-of the) (recurse (third -form-) the))))

(def unwalker the-form (type value)
  `(the ,type ,(recurse value)))

;;;; UNWIND-PROTECT

(def (class* e) unwind-protect-form (walked-form)
  ((protected-form)
   (cleanup-form)))

(def walker unwind-protect
  (with-form-object (unwind-protect 'unwind-protect-form -parent-)
    (setf (protected-form-of unwind-protect) (recurse (second -form-) unwind-protect)
          (cleanup-form-of unwind-protect) (mapcar (lambda (form)
                                                     (recurse form unwind-protect))
                                                   (cddr -form-)))))

(def unwalker unwind-protect-form (protected-form cleanup-form)
  `(unwind-protect ,(recurse protected-form) ,@(recurse-on-body cleanup-form)))

;;;; LOAD-TIME-VALUE

(def (class* e) load-time-value-form (walked-form)
  ((body)
   (read-only nil :accessor read-only? :type boolean)
   (value)))

(def method initialize-instance :after ((self load-time-value-form) &key &allow-other-keys)
  (setf (value-of self) (eval (body-of self))))

(def walker load-time-value
  (assert (<= (length -form-) 3))
  (with-form-object (load-time-value 'load-time-value-form -parent-
                                     :body -form-
                                     :read-only (third -form-))
    (setf (body-of load-time-value) (recurse (second -form-) load-time-value
                                             ;; intentionally walk the body in an empty environment
                                             nil))))

(def unwalker load-time-value-form (body read-only)
  `(load-time-value ,(recurse body) ,@(if read-only '(t))))
