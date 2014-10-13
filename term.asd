#|
  This file is a part of term project.
|#

(in-package :cl-user)
(defpackage term-asd
  (:use :cl :asdf))
(in-package :term-asd)

(defsystem term
  :version "0.1"
  :author ""
  :license ""
  :depends-on (:cl-charms)
  :components ((:module "src"
                :components
                ((:file "term"))))
  :description ""
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
  :in-order-to ((test-op (test-op term-test))))
