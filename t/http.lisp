;;;; -*- lisp -*-

(in-package :it.bese.arnesi.test)

(def-suite :it.bese.arnesi.http :in :it.bese.arnesi)

(in-suite :it.bese.arnesi.http)

(test escape-uri
  (for-all ((uri (gen-string :elements (gen-character :code-limit #16rffff))))
    (is (string= uri (unescape-as-uri (escape-as-uri uri)))))

  (is (string= (unescape-as-uri "a+b+c")
               "a b c")))



