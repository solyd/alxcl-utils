(in-package #:utils)

(defparameter +ascii+ (make-external-format :ascii))

(defgeneric bencoding/encode (item stream)
  (:documentation "Encodes: integer, string, list, dictionary, binary array"))

(defmacro def-encode-without-stream (item-class)
  `(defmethod encode ((item ,item-class) (stream (eql nil)))
     (with-output-to-sequence (out) (encode itme out))))

(defmacro def-encode (item-var item-class stream-var &body body)
  `(def-encode-without-stream (,item-class))
;  `(defmethod encode ((,item-var ,item-class) ,stream-var) ,@body)
  )

(defmethod bencoding/encode ((int integer) (stream (eql nil)))
  (with-output-to-sequence (out) (encode int out)))

(defmethod bencoding/encode ((str string) stream))

(defmethod bencoding/encode ((int integer) stream)
  (write-sequence (string-to-octets (format nil "i~ae" int) :external-format +ascii+) stream))


(defmethod bencoding/encode ((str string) stream)
  (write-sequence (string-to-octets (format nil "~a:" (length str)) :external-format +ascii+) stream)
  (write-sequence (string-to-octets str :external-format :utf-8) stream))

(defmethod bencoding/encode ((list list) stream)
  (write-byte (char-code #\l) stream)
  (loop for item in list do (encode item stream))
  (write-byte (char-code #\e) stream))
