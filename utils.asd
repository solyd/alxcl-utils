;;;; bencoding.asd

(asdf:defsystem #:utils
  :serial t
  :description "My personal collections of utils for Common Lisp"
  :author "Alexander Yavorovsky <alxndr.yav@gmail.com>"
  :license "MIT"
  :depends-on (#:flexi-streams)
  :components ((:file "package")
               (:file "bencoding")
               (:file "debug")))
