;;;; -*- lisp -*-

(in-package :it.bese.arnesi)

(def-suite :it.bese.arnesi.cps :in :it.bese.arnesi)

(in-suite :it.bese.arnesi.cps)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *call/cc-returns* nil))

(test cps-constant
  (is (= 4 (with-call/cc 4)))
  (is (eql :a (with-call/cc :a)))
  (is (eql 'a (with-call/cc 'a))))

(test cps-progn
  (is (null (with-call/cc)))
  (is (= 1 (with-call/cc 1)))
  (is (= 2 (with-call/cc 1 2)))
  (is (= 3 (with-call/cc 1 2 3)))
  (is (= 4 (with-call/cc 1 2 3 4)))
  (is (= 1 (with-call/cc
               (call/cc (lambda (k) (funcall k 1))))))
  (let ((cont nil))
    (is (= 2 (with-call/cc
               1
               (call/cc (lambda (k)
                          (setf cont k)
			  2))
               3)))
    (is (= 3 (funcall cont 42)))))

(test cps-setq
  (let (a cont)
    (is (= 1 (with-call/cc (setq a 1))))
    (is (= 2 (with-call/cc (setq a (1+ a)))))
    (is (= 47 (with-call/cc
                  (+ 5 (setq a (let/cc k
                                       (setf cont k)
                                       (k 42)))))))
    (is (= 42 (funcall cont 37)))))

(test cps-let
  (is (= 1 (with-call/cc
	    (let () 1))))
  (is (= 1 (with-call/cc
	    (let ((a 1)) a))))
  (is (= 1 (let ((a 1)) (with-call/cc a))))

  (let ((a 1))
    (is (= 1 (with-call/cc
               (let ((a 4)
                     (b a))
                 (declare (ignore a))
                 b))))
    (is (= 4 (with-call/cc
               (let ((a 4)
                     (b a))
                 (declare (ignore b))
                 a)))))

  (with-call/cc
    (let ((a 1))
      (let ((a 2))
        (is (= 2 a)))
      (is (= 1 a))))

  (let ((cont nil) 
        (i 0))

    (with-call/cc
     (let ((a (let/cc k (setf cont k) (k 1))))
       (setq i (1+ i))
       (1+ a)))

    (is (= 3 (funcall cont 2)))

    (is (= 2 i))

    (is (= 8 (funcall cont 7)))

    (is (= 3 i))))

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
       (is (= a 47)))))
  (let ((*cont* nil))
    (with-call/cc
      (let* ((a (let/cc k (setf *cont* k)))
	     (b a)
	     (c b))
	(+ a b c)))
    (is (= 3 (funcall *cont* 1)))
    (is (= 6 (funcall *cont* 2)))))

(test cps-apply
  (is (= 0 (with-call/cc (+))))
  (is (= 1 (with-call/cc (+ 1))))
  (is (= 2 (with-call/cc (+ 1 1))))
  (is (= 3 (with-call/cc (+ 1 (+ 1 (+ 1 (+)))))))
  (is (= 10 (let ((cont nil))
	      (is (= 0 (with-call/cc
                           (+ 1 (call/cc (lambda (k)
                                           (setf cont k)
                                           (funcall k 0)))
			       -1))))
              (funcall cont 10)))))

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
                 nil))))

  (let ((cont nil))
    (with-call/cc
      (block foo
	nil
	(let/cc k
	  (setf cont k))
	(return-from foo t)))

    (is (eql t (funcall cont nil)))
    
    (with-call/cc 
      (block foo
        nil
        (return-from foo (let/cc k
                           (setf cont k)))))
    (let ((a (gensym)))
      (is (eq a (funcall cont a))))))

(test cps-flet
  (with-call/cc
   (flet ((a () 'good))
     (flet ((a () 'bad)
	    (b () (a)))
       (is (eql 'good (b)))))))

(test cps-labels
  (with-call/cc
    (labels ((a () 'bad))
      (labels ((a () 'good)
               (b () (a)))
        (is (eql 'good (b))))))
  (let ((cont nil))
    (is (= 2 (with-call/cc
	      (labels ((a () 
		         (let/cc k
			   (setf cont k)
			   (k 0)))
		       (b (whatever)
                         (1+ whatever)))
		(+ 1 (b (a)))))))
    (is (= 18 (funcall cont 16)))))

(test cps-tagbody
  (with-call/cc
    (tagbody a b c)
    (pass))
  (with-call/cc
    (let ((i 0))
      (tagbody
	 a
	 (incf i)
	 (is (= 1 i))
	 b
	 (incf i)
	 (is (= 2 i))
	 c
	 (is (= 2 i)))))
  (let (cont (counter 0))
    (with-call/cc
      (tagbody 
         (let/cc k
           (setf cont k))
         (incf counter)))
    (is (= 0 counter))
    (funcall cont nil)
    (is (= 1 counter))
    (funcall cont nil)    
    (is (= 2 counter)))
  (let (cont (counter 0))
    (with-call/cc
	(let ((a 2))
	  (tagbody
	     (setf a (let/cc k
		       (setf cont k)
		       ;; when we're in here just return a's current
		       ;; value
		       (k a)))
	   start
	     (incf counter)
	     (decf a)
	     (when (zerop a)
	       (go done))
	     (go start)
	   done)
	  (is (zerop a))))
    ;; first time counter should be 2
    (is (= 2 counter))
    (funcall cont 4)
    ;; should make use go throguh the tagbody 4 times.
    (is (= 6 counter))))

(test cps-general1
  (with-call/cc
    (let ((i 1))
      (decf i)
      (is (zerop i))))
  (with-call/cc
    (let ((value 'foo))
      (case value
        (foo (pass))
        (t (fail)))))
  (let ((cont1 nil)
        (cont2 nil)
        (i 0))
    (with-call/cc
      (case (let/cc k
              (setf cont1 k)
              'foo)
        (foo (pass))
        (t (let/cc k
                   (setf cont2 k)
                   (k 'whatever))
           (incf i))))
    (is (= 0 i))
    (is (= 1 (funcall cont1 'bar)))
    (is (= 2 (funcall cont2 nil)))
    (is (= 3 (funcall cont2 nil)))))

(defvar *cont* nil)

(defun/cc foo (x)
  (+ x (let/cc k
         (setf *cont* k)
         (funcall k 0))))

(test defun/cc
  (setf *cont* nil)
  (is (= 4 (with-call/cc
             (+ 2 (foo 2)))))
  (is (= 4 (funcall *cont* 0))))
