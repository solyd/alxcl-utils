;;;; bencoding.asd

(asdf:defsystem #:utils
  :serial t
  :description "My personal collections of utils for Common Lisp"
  :author "Alexander Yavorovsky <alxndr.yav@gmail.com>"
  :license "MIT"
  :depends-on (#:flexi-streams
               #:log4cl)
  :components ((:file "package")
               (:file "utils")
               (:file "bencoding"
                      :depends-on ("package" "utils"))
               (:file "debug")))
