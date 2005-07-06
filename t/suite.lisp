;;;; -*- lisp -*-

(in-package :it.bese.arnesi)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unintern 'arnesi::test (find-package :it.bese.arnesi)))

(use-package :it.bese.FiveAM)

(unless (5am:get-test :it.bese)
  (5am:def-suite :it.bese))

(5am:def-suite :it.bese.arnesi :in :it.bese)
