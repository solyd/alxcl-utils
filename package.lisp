(defpackage #:alxcl-utils
  (:use #:cl)

  (:export :hexdump
           :hexdump-to-stream
           :log-hexdump

           :bencoding/encode
           :bencoding/decode

           :ipv4-vec->string
           :endpoint-address)

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
