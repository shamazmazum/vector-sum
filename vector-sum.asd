(defsystem :vector-sum
  :name :vector-sum
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :description "numpy.sum() for a vector"
  :licence "2-clause BSD"
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "vector-sum"))
  :depends-on (:serapeum :alexandria)
  :in-order-to ((test-op (load-op "vector-sum/tests")))
  :perform (test-op (op system)
                    (declare (ignore op system))
                    (funcall
                     (symbol-function
                      (intern (symbol-name '#:run-tests)
                              (find-package :vector-sum/tests))))))

(defsystem :vector-sum/tests
  :name :vector-sum/tests
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :licence "2-clause BSD"
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "tests"))
  :depends-on (:vector-sum :fiveam))

