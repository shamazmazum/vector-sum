(in-package :vector-sum/tests)

(defun run-tests ()
  (explain! (run 'vector-sum)))

(def-suite vector-sum :description "Test vector-sum")
(in-suite vector-sum)

(declaim (inline complex-real->double))
(defun complex-real->double (x)
  (complex
   (float (realpart x) 0d0)
   (float (imagpart x) 0d0)))

(declaim (inline complex-real->float))
(defun complex-real->float (x)
  (complex
   (float (realpart x) 0f0)
   (float (imagpart x) 0f0)))

(test vector-of-singles
  (loop repeat 10000
        for xs = (make-array 5000 :element-type 'single-float :initial-contents
                             (loop repeat 5000 collect (random 1.0)))
        for s1 = (vector-sum:sum xs)
        for s2 = (float
                  (reduce #'+ (map '(vector double-float)
                                   (lambda (x) (float x 0d0))
                                   xs))
                  0f0)
        do (is (= s1 s2))))

(test vector-of-complex-singles
  (loop repeat 10000
        for xs = (make-array 5000 :element-type '(complex single-float) :initial-contents
                             (loop repeat 5000 collect (complex (random 1.0) (random 1.0))))
        for s1 = (vector-sum:sum xs)
        for s2 = (complex-real->float
                  (reduce #'+ (map '(vector (complex double-float))
                                   #'complex-real->double
                                   xs)))
        do (is (= s1 s2))))
