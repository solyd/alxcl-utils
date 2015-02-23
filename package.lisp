;;;; package.lisp

(defpackage #:utils
  (:use #:cl)
  (:export :hexdump)

  (:import-from #:flexi-streams
                #:flexi-stream
                #:peek-byte
                #:make-flexi-stream
                #:make-external-format
                #:make-in-memory-input-stream
                #:with-output-to-sequence
                #:with-input-from-sequence
                #:string-to-octets
                #:octets-to-string))
