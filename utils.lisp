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

(defun read-byte-if-equals (stream byte)
  "Reads byte from stream if it equals to provided byte"
  (if (eql (peek-byte stream nil nil nil) byte)
      (read-byte stream :eof-error-p nil)
      nil))

(defun read-ascii-digits (stream)
  "Reads base 10 numbers from stream, stopping when encoutering some
other char. Returns string with digits"
  (with-output-to-string (result)
    (loop for byte = (peek-byte stream) while (digit-char-p (code-char byte)) do
         (write-char (code-char (read-byte stream)) result))))

(defun read-octets (stream num)
  (let* ((result (make-array num :element-type '(unsigned-byte 8)))
         (num-read (read-sequence result stream)))
    (assert (= num num-read) (num)
            "Failed to read ~d octets (actualyread: ~d)" num num-read)
    result))
