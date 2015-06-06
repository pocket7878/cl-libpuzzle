#|
  This file is a part of cl-libpuzzle project.
  Copyright (c) 2013 Masato Sogame (poketo7878@gmail.com)
|#

#|
  Author: Masato Sogame (poketo7878@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-libpuzzle-asd
  (:use :cl :asdf))
(in-package :cl-libpuzzle-asd)

(defsystem cl-libpuzzle
  :version "0.1"
  :author "Masato Sogame"
  :license "LLGPL"
  :depends-on (:cffi)
  :components ((:module "src"
                :components
                ((:file "cl-libpuzzle"))))
  :description "libpuzzle(http://www.pureftpd.org/project/libpuzzle) binding for Common Lisp"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op cl-libpuzzle-test))))
