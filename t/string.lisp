;;;; -*- lisp -*-

(in-package :it.bese.arnesi.test)

(def-suite :it.bese.arnesi.string :in :it.bese.arnesi)

(in-suite :it.bese.arnesi.string)

(test trim-string
  (is (string= "4" (trim-string "   4    ")))
  (is (string= "4" (trim-string "4")))
  (is (string= "4" (trim-string "aa4" #\a)))
  (is (string= "4" (trim-string "aa4aa" #\a)))
  (is (string= "4" (trim-string " a4a" `(#\a #\Space)))))
