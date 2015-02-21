;;;; package.lisp

(defpackage #:utils
  (:use #:cl)
  (:import-from #:flexi-streams
                #:make-external-format
                #:with-output-to-sequence
                #:string-to-octets
                #:octets-to-string))
