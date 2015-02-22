(in-package #:utils)

(define-condition unexpected-byte (error)
  ((expected-byte :initarg :expected-byte
                  :reader expected-byte)
   (actual-byte :initarg :actual-byte
                :reader actual-byte)))

(defun read-expected-byte (stream expected-byte)
  "Reads byte from stream, which must be equal to provided byte.
If the byte mismatches, unexpected-bye error is raised"
  (let ((actual-byte (read-byte stream)))
    (if (eql actual-byte expected-byte)
        expected-byte
      (error 'unexpected-byte :expected-byte expected-byte :actual-byte actual-byte))))

(defun read-possible-byte (stream possible-byte)
  "Reads byte from stream, if it's the byte provided returns t,
  otherwise un-reads the byte and returns nil"
  (if (eql (peek-byte stream nil t) possible-byte)
      (read-byte stream :eof-error-p t)
    nil))

(defun read-ascii-digits (stream)
  (with-output-to-string
    (result)
    (loop for byte = (peek-byte stream) while (digit-char-p (char-code byte)) do
          (write-char (code-char (read-byte stream)) result))))
