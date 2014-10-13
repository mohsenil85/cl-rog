#|
  This file is a part of term project.
|#

(in-package :cl-user)
(defpackage term-test-asd
  (:use :cl :asdf))
(in-package :term-test-asd)

(defsystem term-test
  :author ""
  :license ""
  :depends-on (:term
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "term"))))

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
