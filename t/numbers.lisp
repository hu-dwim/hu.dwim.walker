;;;; -*- lisp -*-

(in-package :it.bese.arnesi)

(def-suite :it.bese.arnesi.numbers :in :it.bese.arnesi)

(in-suite :it.bese.arnesi.numbers)

(test mulf
  (let ((a 0))
    (is (= 0 (mulf a 10)))
    (is (= 0 a)))

  (let ((a 1))
    (is (= 4 (mulf a 4)))
    (is (= 1 (mulf a (/ 4))))
    (is (= 1 a))))

(test minf
  (let ((a 10))
    (is (= 5 (minf a 5)))
    (is (= 5 a)))

  (let ((a 0))
    (is (= 0 (minf a 10)))
    (is (= 0 a))))

(test parse-float
  (is (= 0 (parse-float "0")))
  (is (= -1 (parse-float "-1")))
  (is (= 1 (parse-float "1")))

  (dolist (type '(short-float single-float double-float long-float))
    (dotimes (i 1000)
      (let* ((*print-radix* 10)
             (number (random  (1+ (coerce i type))))
             (string (princ-to-string number))
             (value (parse-float string :type type)))
        (unless (= number value)
          (fail "~S parse-float'd to ~S (not ~S)." string value number))))))

