#|
  This file is a part of cl-libhoedown project.
  Copyright (c) 2016 David Gu (david_guru@gty.org.in)
|#

#|
  Common Lisp Binding for Hoedown.

  Author: David Gu (david_guru@gty.org.in)
|#

(in-package :cl-user)
(defpackage cl-libhoedown-asd
  (:use :cl :asdf))
(in-package :cl-libhoedown-asd)

(defsystem cl-libhoedown
  :version "0.1"
  :author "David Gu"
  :license ""
  :depends-on (:cffi)
  :components ((:module "src"
                :components
                ((:file "cl-libhoedown"))))
  :description "Common Lisp Binding for Hoedown."
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
          seq))))
