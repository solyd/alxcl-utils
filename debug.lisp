(in-package :utils)

(defun hexdump (seq &key (address-length 8) (address-offset 0))
  (labels ((x->char (x)
             (let ((c (code-char x)))
               (if (and (standard-char-p c) (graphic-char-p c)) c #\.)))

           (x->str (l)
             (coerce (mapcar #'x->char l) 'string))

           (print-header ()
             (princ (make-string address-length :initial-element #\=))
             (let ((l '#.(loop for n below 16 collect n)))
               (format t "== ~{+~x ~}==================~%" l)))

           (print-address (i)
             (format t (format nil "~~~a,'0x : " address-length)
                     (+ address-offset i)))

           (print-byte (b)
             (format t "~2,'0x " b))

           (print-text (l)
             (format t "| ~a~%" (x->str l)))

           (print-padding (n)
             (princ (make-string (* n 3) :initial-element #\space)))

           (reduce-fn (state byte)
             (destructuring-bind (col row txt) state
               (when (and (zerop col) (zerop (mod row 10)))
                 (print-header))
               (when (zerop col)
                 (print-address (* row 16)))
               (print-byte byte)
               (when (= col 15)
                 (print-text (nreverse (cons byte txt))))
               (if (= col 15)
                   (list 0 (1+ row) nil)
                   (list (1+ col) row (cons byte txt))))))
    (fresh-line)
    (destructuring-bind (col row txt)
        (reduce #'reduce-fn seq :initial-value (list 0 0 nil))
      (declare (ignore row))
      (unless (zerop col)
        (print-padding (- 16 col))
        (print-text (nreverse txt))))
    (fresh-line)))
