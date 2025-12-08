(in-package :vector-sum/tests)

(defun run-tests ()
  (every #'identity
         (mapcar (lambda (suite)
                   (let ((status (run suite)))
                     (explain! status)
                     (results-status status)))
                 '(sum scan v1api))))

(def-suite sum   :description "Test SUM")
(def-suite scan  :description "Test SCAN")
(def-suite v1api :description "Version 1 API")

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

(in-suite sum)
(test vector-of-singles/sum
  (loop repeat 10000
        for xs = (make-array 5000 :element-type 'single-float :initial-contents
                             (loop repeat 5000 collect (random 1.0)))
        for s1 = (vector-sum:sum xs)
        for s2 = (float (reduce #'+ xs :key (lambda (x) (float x 0d0))) 0f0)
        do (is (= s1 s2))))

(test vector-of-complex-singles/sum
  (loop repeat 10000
        for xs = (make-array 5000 :element-type '(complex single-float) :initial-contents
                             (loop repeat 5000 collect (complex (random 1.0) (random 1.0))))
        for s1 = (vector-sum:sum xs)
        for s2 = (complex-real->float
                  (reduce #'+ xs :key #'complex-real->double))
        do (is (= s1 s2))))

(in-suite scan)
(test vector-of-singles/scan
  (loop repeat 10000
        for xs = (make-array 5000 :element-type 'single-float :initial-contents
                             (loop repeat 5000 collect (random 1.0)))
        for s1 = (vector-sum:scan xs)
        for s2 = (map '(vector single-float)
                      (lambda (x) (float x 0f0))
                      ;; This function does not use Kahan summation algo
                      (vector-sum::scan/rational
                       (map '(vector double-float)
                            (lambda (x) (float x 0d0))
                            xs)))
        do (is (equalp s1 s2))))

(test vector-of-complex-singles/scan
  (loop repeat 10000
        for xs = (make-array 5000 :element-type '(complex single-float) :initial-contents
                             (loop repeat 5000 collect (complex (random 1.0) (random 1.0))))
        for s1 = (vector-sum:scan xs)
        for s2 = (map '(vector (complex single-float))
                      #'complex-real->float
                      (vector-sum::scan/rational
                       (map '(vector (complex double-float))
                            #'complex-real->double
                            xs)))
        do (is (equalp s1 s2))))

(in-suite v1api)
(test singles/sum
  (loop repeat 10000
        for xs = (make-array 5000 :element-type 'single-float :initial-contents
                             (loop repeat 5000 collect (random 1.0)))
        for s1 = (vector-sum:state-sum
                  (reduce #'vector-sum:add xs :initial-value (vector-sum:sum-state 0f0)))
        for s2 = (float (reduce #'+ xs :key (lambda (x) (float x 0d0))) 0f0)
        do (is (= s1 s2))))

(test complex-singles/sum
  (loop repeat 10000
        for xs = (make-array 5000 :element-type '(complex single-float) :initial-contents
                             (loop repeat 5000 collect (complex (random 1.0) (random 1.0))))
        for s1 = (vector-sum:state-sum
                  (reduce #'vector-sum:add xs :initial-value (vector-sum:sum-state
                                                              (complex 0f0))))
        for s2 = (complex-real->float
                  (reduce #'+ xs :key #'complex-real->double))
        do (is (= s1 s2))))
