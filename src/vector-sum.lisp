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
                                alexandria:non-negative-fixnum
                                alexandria:non-negative-fixnum
                                ,@(if with-key-p
                                      `((function (,type) (values ,type &optional)))))
                               (values ,type &optional))
                  (defun ,name (xs start end ,@(if with-key-p '(f)))
                    (declare (optimize (speed 3)))
                    ;; Rebind end to elliminate bounds checks
                    (let ((end (min (length xs) end)))
                      (loop with sum of-type ,type = (coerce 0 ',type)
                            with err of-type ,type = (coerce 0 ',type)
                            for i from start below end
                            for x = (aref xs i) do
                            (let* ((y (- ,(if with-key-p '(funcall f x) 'x) err))
                                   (tmp (+ sum y)))
                              (setq err (- tmp sum y)
                                    sum tmp))
                            finally (return sum))))))))
  (define-summator single-float nil)
  (define-summator double-float nil)
  (define-summator single-float t)
  (define-summator double-float t)
  (define-summator (complex single-float) nil)
  (define-summator (complex double-float) nil)
  (define-summator (complex single-float) t)
  (define-summator (complex double-float) t))


(serapeum:-> sum ((simple-array * (*)) &key
                  (:key   (or function null))
                  (:start alexandria:non-negative-fixnum)
                  (:end   alexandria:non-negative-fixnum))
             (values number &optional))
(declaim (inline sum))
(defun sum (xs &key (key nil has-key-p) (start 0) (end (length xs)))
  "Sum all elements in a vector XS from START (inclusive) to END
(exclusive), applying F to each element before summation.

(sum xs)                 == (reduce #'+ xs)
(sum xs :key f)          == (reduce #'+ xs :key f)
(sum xs :start n :end m) == (reduce #'+ xs :start n :end m)"
  (let ((type (array-element-type xs)))
    (if has-key-p
        (funcall
         (cond
           ((equalp type 'single-float)
            #'sum/single-float/f)
           ((equalp type 'double-float)
            #'sum/double-float/f)
           ((equalp type '(complex single-float))
            #'sum/complex-single-float/f)
           ((equalp type '(complex double-float))
            #'sum/complex-double-float/f)
           (t
            ;; Ordinary sum of rationals
            (lambda (xs start end f)
              (reduce #'+ xs :key f :start start :end end))))
         xs start end key)
        (funcall
         (cond
           ((equalp type 'single-float)
            #'sum/single-float)
           ((equalp type 'double-float)
            #'sum/double-float)
           ((equalp type '(complex single-float))
            #'sum/complex-single-float)
           ((equalp type '(complex double-float))
            #'sum/complex-double-float)
           (t
            ;; Ordinary sum of rationals
            (lambda (xs start end)
              (reduce #'+ xs :start start :end end))))
         xs start end))))
