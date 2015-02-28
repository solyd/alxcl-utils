(in-package #:alxcl-utils)

(defclass endpoint-address ()
  ((host :initarg :host
         :reader host)
   (port :initarg :port
         :reader port)))

(defmethod print-object ((address endpoint-address) stream)
  (let ((host (host address)))
    (if (stringp host)
        (format stream "~a:~d" host (port address))
      (format stream "~a:~d" (ipv4-vec->string host) (port address)))))

(defun ipv4-vec->string (ipv4-vec)
  "Converts 4 byte vector to ipv4 string address"
  (with-output-to-string (result)
    (write-string (write-to-string (aref ipv4-vec 0)) result)
    (write-string "." result)
    (write-string (write-to-string (aref ipv4-vec 1)) result)
    (write-string "." result)
    (write-string (write-to-string (aref ipv4-vec 2)) result)
    (write-string "." result)
    (write-string (write-to-string (aref ipv4-vec 3)) result)))

(defun ipv4-vec->endpoint-address (ipv4-vec)
  (make-instance 'endpoint-address
                 :host (subseq ipv4-vec 0 4)
                 :port (octets->uint16 (subseq ipv4-vec 4 6))))
