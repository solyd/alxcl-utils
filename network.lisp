(in-package #:alxcl-utils)

(defclass endpoint-address ()
  ((host :initarg :host
         :reader host)
   (port :initarg :port
         :reader port)))

(defmethod print-object ((address endpoint-address) stream)
  (let ((host (host address)))
    (if (vectorp host)
        (format stream "~a:~d" (ipv4-vec->string host) (port address))
        (format stream "~a:~d" host (port address)))))
