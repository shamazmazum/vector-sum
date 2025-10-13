(defun do-all ()
  (ql:quickload :vector-sum/tests)
  (uiop:quit
   (if (uiop:call-function "vector-sum/tests:run-tests")
       0 1)))

(do-all)
