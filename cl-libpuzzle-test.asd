#|
  This file is a part of cl-libpuzzle project.
  Copyright (c) 2013 Masato Sogame (poketo7878@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-libpuzzle-test-asd
  (:use :cl :asdf))
(in-package :cl-libpuzzle-test-asd)

(defsystem cl-libpuzzle-test
  :author "Masato Sogame"
  :license ""
  :depends-on (:cl-libpuzzle
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:file "cl-libpuzzle"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
