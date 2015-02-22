;;;; package.lisp

(defpackage #:utils
  (:use #:cl)
  (:export :hexdump)

  (:import-from #:flexi-streams
                #:flexi-stream
                #:peek-byte
                #:make-flexi-stream
                #:make-external-format
                #:with-output-to-sequence
                #:string-to-octets
                #:octets-to-string))
