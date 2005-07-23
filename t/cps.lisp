;;;; -*- lisp -*-

(in-package :it.bese.arnesi)

(def-suite :it.bese.arnesi.cps :in :it.bese.arnesi)

(in-suite :it.bese.arnesi.cps)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *call/cc-returns* nil))

(test cps-constant
  (is (= 4 (with-call/cc 4)))
  (is (eql :a (with-call/cc :a)))
  (is (eql 'a (with-call/cc 'a)))
  (is (eql #'+ (with-call/cc #'+))))

(test cps-progn
  (is (null (with-call/cc)))
  (is (= 1 (with-call/cc 1)))
  (is (= 2 (with-call/cc 1 2)))
  (is (= 3 (with-call/cc 1 2 3)))
  (is (= 4 (with-call/cc 1 2 3 4))))

(test cps-progn/cc
  (is (= 1 (kall (with-call/cc (let/cc k k) 1))))
  (is (= 1 (kall (with-call/cc (let/cc k k) 0 1)))))

(test cps-let
  (is (= 1 (with-call/cc
	    (let () 1))))
  (is (= 1 (with-call/cc
	    (let ((a 1)) a))))
  (is (= 1 (with-call/cc
             (let ((a 4)
                   (b a))
               (declare (ignore a))
               b))))
  (is (= 4 (with-call/cc
             (let ((a 4)
                   (b a))
               (declare (ignore b))
               a))))
  (with-call/cc
    (let ((a 1))
      (let ((a 2))
        (is (= 2 a)))
      (is (= 1 a))))

  (let ((cont nil))
    (setf cont
          (with-call/cc
            (let ((a (let/cc k k)))
              (+ a 4))))
    (is (= 9 (kall cont 5)))
    (is (= 12 (kall cont 8)))))

(test cps-let/cc
  (let ((k (with-call/cc
             (let ((a (retk)))
               (+ a 1)))))
  (is (= 1 (kall k 0)))
  (is (= 2 (kall k 1)))))

(test cps-setq
  (is (= 1 (with-call/cc
             (let ((a nil)) (setq a 1)))))
  (is (= 2 (with-call/cc
             (let ((a 1)) (setq a (1+ a)))))))

(test cps-let*
  (with-call/cc
    (let* ((a 1)
	   (b a))
      (is (= 1 a))
      (is (= 1 b))))
  (with-call/cc
   (let ((a 0)
	 (b 1))
     (declare (ignore a))
     (let* ((a b)
	    (b a))
       (is (= a 1))
       (is (= b 1))
       (setq a 47)
       (is (= a 47))))))

(test cps-apply
  (is (= 0 (with-call/cc (+))))
  (is (= 1 (with-call/cc (+ 1))))
  (is (= 2 (with-call/cc (+ 1 1))))
  (is (= 3 (with-call/cc (+ 1 (+ 1 (+ 1 (+))))))))

(test cps-if
  (is (= 1 (with-call/cc (if t 1))))
  (is (= 1 (with-call/cc (if nil 0 1))))
  (is (null (with-call/cc (if nil 1)))))

(test cps-block/return-from
  (is (= 1
         (with-call/cc
           (block foo
               nil
               (return-from foo 1)
               nil))))
  (is (eql t 
           (with-call/cc
               (block foo
                 (return-from foo t)
                 nil)))))

(defun reached-unreachable-code ()
  (fail "Somehow we reached unreachable code in a tagbody."))

(test cps-tagbody
  (with-call/cc
    (tagbody
       (go a)
       (reached-unreachable-code)
       a
       (pass)))
  (with-call/cc
    (tagbody
       (go a) (reached-unreachable-code)
     b
       (pass)
       (go c) (reached-unreachable-code)
     a
       (pass)
       (go b) (reached-unreachable-code)
     c
       (pass)))
  (with-call/cc
    (let ((counter 0))
      (dotimes (i 5)
        (incf counter))
      (is (= 5 counter))))
  (with-call/cc
    (let ((i 0))
      (tagbody
       a (incf i) (is (= 1 i))
       b (incf i) (is (= 2 i))
       c (is (= 2 i))))))

(test cps-flet
  (with-call/cc
    (flet ((foo () 'x))
      (is (eql 'x (foo))))
    (is (= 4 (funcall (let ((a 4))
                        (flet ((foo () a))
                          #'foo)))))
    (flet ((foo ()
             'outer-foo))
      (flet ((foo ()
               'inner-foo)
             (bar ()
               (foo)))
        (is (eql 'outer-foo (bar)))))))

(test cps-labels
  (with-call/cc
    (labels ((foo () 'x))
      (is (eql 'x (foo))))
    (labels ((foo () 'outer-foo))
      (labels ((bar () (foo))
               (foo () 'inner-foo))
        (is (eql 'inner-foo (bar)))))))

(let ((value 0))
  (defun test-funcall.0 ()
    value)
  (defun (setf test-funcall.0) (new-value)
    (setf value new-value)))

(test cps-setf-funcall
  (is (= 0 (with-call/cc (test-funcall.0))))
  (is (= 1 (with-call/cc (setf (test-funcall.0) 1))))
  (is (= 2 (with-call/cc (funcall #'(setf test-funcall.0) 2)))))

(test cps-lambda-requried-arguments
  (with-call/cc
    (is (eql t (funcall (lambda () t))))
    (is (eql t (funcall (lambda (x) x) t))))
  (signals error
    (with-call/cc
      (funcall (lambda (x) x)))))

(test cps-lambda-optional-arguments
  (with-call/cc
    (is (eql t (funcall (lambda (&optional a) a) t)))
    (is (eql t (funcall (lambda (&optional (a t)) a)))))

  (let ((cont (with-call/cc
                (funcall (lambda (&optional (a (let/cc k k)))
                           (+ a 1))))))
    (is (= 1 (kall cont 0)))))

(defun/cc test-defun/cc1 ()
  (let/cc k k))

(defun/cc test-defun/cc2 (arg1)
  (let/cc k k)
  arg1)

(test cps-defun/cc
  (let ((cont nil))
    (setf cont (with-call/cc (test-defun/cc1)))
    (is (eql nil (kall cont nil)))

    (setf cont (with-call/cc (test-defun/cc2 'foo)))
    (is (eql 'foo (kall cont)))
    (is (eql 'foo (kall cont nil)))))
