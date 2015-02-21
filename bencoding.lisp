(in-package #:utils)

(defparameter +ascii+ (make-external-format :ascii))

(define-condition invalid-dict-key (error)
  ((key :initarg :key
        :accessor key)))

(defun dict->sorted-list (dict)
  (let ((result))
    (maphash (lambda (key val)
               (if (stringp key)
                   (push (cons key val) result)
                   (error 'invalid-dict-key :key key)))
             dict)
    (sort result #'string< :key #'car)))

;; Encoding
;; ================================================================================

(defgeneric bencoding/encode (item)
  (:documentation "Encodes: integer, string, list, dictionary, binary array"))

(defgeneric bencoding/encode-to-stream (item stream)
  (:documentation "Encodes: integer, string, list, dictionary, binary array"))

(def-encode integer)
(def-encode string)
(def-encode array)
(def-encode list)
(def-encode hash-table)

(defmacro def-encode (item-class)
  `(defmethod bencoding/encode ((item ,item-class))
     (bencoding/encode-to-stream item nil)))

(defmacro def-encode-without-stream (item-class)
  `(defmethod bencoding/encode-to-stream ((item ,item-class) (stream (eql nil)))
     (with-output-to-sequence (out) (bencoding/encode-to-stream item out))))

(defmacro def-encode-to-stream ((item-var item-class stream-var) &body body)
  `(progn
     (def-encode-without-stream ,item-class)
     (defmethod bencoding/encode-to-stream ((,item-var ,item-class) ,stream-var) ,@body)))

(def-encode-to-stream (int integer stream)
  (write-sequence (string-to-octets (format nil "i~ae" int) :external-format +ascii+) stream))

(def-encode-to-stream (str string stream)
  (write-sequence (string-to-octets (format nil "~a:" (length str)) :external-format +ascii+) stream)
  (write-sequence (string-to-octets str :external-format :utf-8) stream))

(def-encode-to-stream (arr array stream)
  (write-sequence (string-to-octets (format nil "~a:" (length arr)) :external-format +ascii+) stream)
  (write-sequence arr stream))

(def-encode-to-stream (list list stream)
  (write-byte (char-code #\l) stream)
  (loop for item in list do (bencoding/encode-to-stream item stream))
  (write-byte (char-code #\e) stream))

(def-encode-to-stream (dict hash-table stream)
  (write-byte (char-code #\d) stream)
  (loop for keyval in (dict->sorted-list dict) do
       (destructuring-bind (key . val) keyval
         (bencoding/encode-to-stream key stream)
         (bencoding/encode-to-stream val stream)))
  (write-byte (char-code #\e) stream))

;; Decoding
;; ================================================================================
