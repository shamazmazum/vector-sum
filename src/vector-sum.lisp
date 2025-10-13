;; https://en.wikipedia.org/wiki/Kahan_summation_algorithm
(in-package :vector-sum)

;; Unfortunately, SBCL cannot infer the result type of %GO if we just inline it
(macrolet ((define-summator (type with-key-p)
             (let* ((type-name
                     (cond
                       ((atom type)
                        (symbol-name type))
                       ((eq (first type) 'complex)
                        (format nil "COMPLEX-~a" (second type)))
                       (t
                        (error "Something wrong"))))
                    (name (intern (format nil "SUM/~a~:[~;/F~]" type-name with-key-p))))
               `(progn
                  (serapeum:-> ,name
                               ((simple-array ,type (*))
                                ,@(if with-key-p
                                      `((function (,type) (values ,type &optional)))))
                               (values ,type &optional))
                  (defun ,name (xs ,@(if with-key-p '(f)))
                    (declare (optimize (speed 3)))
                    (loop with sum of-type ,type = (coerce 0 ',type)
                          with err of-type ,type = (coerce 0 ',type)
                          for x across xs do
                          (let* ((y (- ,(if with-key-p '(funcall f x) 'x) err))
                                 (tmp (+ sum y)))
                            (setq err (- tmp sum y)
                                  sum tmp))
                          finally (return sum)))))))
  (define-summator single-float nil)
  (define-summator double-float nil)
  (define-summator single-float t)
  (define-summator double-float t)
  (define-summator (complex single-float) nil)
  (define-summator (complex double-float) nil)
  (define-summator (complex single-float) t)
  (define-summator (complex double-float) t))


(serapeum:-> sum ((simple-array * (*)) &optional (or function null))
             (values number &optional))
(declaim (inline sum))
(defun sum (xs &optional (f nil has-f-p))
  "Sum all elements in a vector XS, applying F to each element before summation.

(sum xs)   == (reduce #'+ xs)
(sum xs f) == (reduce #'+ xs :key f)"
  (let ((type (array-element-type xs)))
    (if has-f-p
        (cond
          ((equalp type 'single-float)
           (sum/single-float/f xs f))
          ((equalp type 'double-float)
           (sum/double-float/f xs f))
          ((equalp type '(complex single-float))
           (sum/complex-single-float/f xs f))
          ((equalp type '(complex double-float))
           (sum/complex-double-float/f xs f))
          (t
           ;; Ordinary sum of rationals
           (reduce #'+ xs :key f)))
        (cond
          ((equalp type 'single-float)
           (sum/single-float xs))
          ((equalp type 'double-float)
           (sum/double-float xs))
          ((equalp type '(complex single-float))
           (sum/complex-single-float xs))
          ((equalp type '(complex double-float))
           (sum/complex-double-float xs))
          (t
           ;; Ordinary sum of rationals
           (reduce #'+ xs))))))
