(in-package :it.bese.arnesi.test)

(def-suite :it.bese.arnesi.sharpl :in :it.bese.arnesi)

(in-suite :it.bese.arnesi.sharpl)

(eval-when (:compile-toplevel :execute)
  (enable-sharp-l))

(test sharpl-simple
  (is (eql 42 (funcall (arnesi::sharpl-expander 42 0)))))

(test sharpl-mb-example
  (is (eql 6 (funcall (arnesi::sharpl-expander (block !2 (return-from !2 !1)) 0) 6))))

(test sharpl-finds-variables
  (is (eql 111 (funcall (arnesi::sharpl-expander (+ !1 !2) 0) 42 69))))

(test sharpl-no-variable-in-quote
  (is (eq (funcall (arnesi::sharpl-expander '!1 0)) '!1)))

(test sharpl-not-captures-outer-bang
  (let ((!1 42))
    (declare (ignore !1))
    (is (eql 69 (funcall (arnesi::sharpl-expander !1 0) 69)))))

(test sharpl-nested-simple
  (is (eql 1 (funcall (funcall (arnesi::sharpl-expander (arnesi::sharpl-expander 1 0) 0))))))

(test sharpl-nested-arg
  (is (eql 42 (funcall (funcall (arnesi::sharpl-expander (arnesi::sharpl-expander !1 0) 0)) 42))))

(test sharpl-nested-complex
  (is (eql 3 (funcall 
	      (funcall (arnesi::sharpl-expander (let ((a !1)) 
			   (arnesi::sharpl-expander (+ !1 a) 0)) 0)
		       1)
	      2))))

(test sharpl-symbol-macrolet-1
  (is (eql 3 (symbol-macrolet ((sym !1)) (funcall (arnesi::sharpl-expander sym 0) 3)))))

(test sharpl-symbol-macrolet-2
  (is (eql 3 (funcall (symbol-macrolet ((sym !1)) (arnesi::sharpl-expander sym 0)) 3 ))))

(test sharpl-macrolet-1
  (is (eql 15 (macrolet ((mac (arg) `(+ !1 ,arg))) (funcall (arnesi::sharpl-expander (mac 10) 0) 5)))))

(test sharpl-macrolet-2
  (is (eql 15 (funcall (macrolet ((mac (arg) `(+ !1 ,arg))) (arnesi::sharpl-expander (mac 10) 0)) 5))))

(test sharpl-inner-macrolet
  (is (eql 15 (funcall 
	       (arnesi::sharpl-expander 
		(macrolet ((!2 () '!1)) (!2))
		0)
	       15))))

(test sharpl-inner-symbol-macrolet
  (is (eql 15 (funcall 
	       (arnesi::sharpl-expander 
		(symbol-macrolet ((!2 !1)) (+ !2 10))
		0)
	       5))))

(test sharpl-bang-binds-to-innermost
  (is (eql 10 (funcall 
	       (funcall (arnesi::sharpl-expander
			 (let ((a !1))
			   (arnesi::sharpl-expander (+ a !1) 0)) 0)
			6)
	       4))))

(test sharpl-interposed-macrolet
  (is (eql 6 (funcall
              (funcall (arnesi::sharpl-expander
                        (macrolet ((mac () '!1))
                          (arnesi::sharpl-expander (mac) 0))
                        0))
              6))))

(test sharpl-nested-macrolet
  (is (eql 21 (funcall
               (funcall
                (arnesi::sharpl-expander
                 (macrolet ((return-bang () ''!1))
                   (macrolet ((multiply-first-bang (arg) `(* ,arg ,(return-bang))))
                     (arnesi::sharpl-expander (+ (multiply-first-bang 2) 1) 0)))
                 0))
               10))))
                          
(test sharpl-interposed-symbol-macrolet
  (is (eql 'result (funcall
              (funcall (arnesi::sharpl-expander
                        (symbol-macrolet ((mac !1))
                          (arnesi::sharpl-expander mac 0))
                        0))
              'result))))
  
