;;;; -*- lisp -*-

(in-package :it.bese.arnesi)

(def-suite :it.bese.arnesi.http :in :it.bese.arnesi)

(in-suite :it.bese.arnesi.http)

(test escape-uri
  (for-all ((uri (gen-string :elements (gen-character))))
    (is (string= uri (unescape-as-uri (escape-as-uri uri))))))



