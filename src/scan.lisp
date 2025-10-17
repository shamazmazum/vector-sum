(in-package :vector-sum)

;; This is mostly copy-pasta from sum.lisp
(macrolet ((define-scan (type with-key-p)
             (let* ((type-name
                     (cond
                       ((atom type)
                        (symbol-name type))
                       ((eq (first type) 'complex)
                        (format nil "COMPLEX-~a" (second type)))
                       (t
                        (error "Something wrong"))))
                    (name (intern (format nil "SCAN/~a~:[~;/F~]" type-name with-key-p))))
               `(progn
                  (serapeum:-> ,name
                               ((simple-array ,type (*))
                                ,@(if with-key-p
                                      `((function (,type) (values ,type &optional)))))
                               (values (simple-array ,type (*)) &optional))
                  (defun ,name (xs ,@(if with-key-p '(f)))
                    (declare (optimize (speed 3)))
                    (loop with result = (make-array (length xs) :element-type ',type)
                          with sum of-type ,type = (coerce 0 ',type)
                          with err of-type ,type = (coerce 0 ',type)
                          for i below (length xs)
                          for x = (aref xs i) do
                          (let* ((y (- ,(if with-key-p '(funcall f x) 'x) err))
                                 (tmp (+ sum y)))
                            (setf err (- tmp sum y)
                                  sum tmp
                                  (aref result i) sum))
                          finally (return result)))))))
  (define-scan single-float nil)
  (define-scan double-float nil)
  (define-scan single-float t)
  (define-scan double-float t)
  (define-scan (complex single-float) nil)
  (define-scan (complex double-float) nil)
  (define-scan (complex single-float) t)
  (define-scan (complex double-float) t))

(macrolet ((define-scan (with-key-p)
             (let ((name (intern (if with-key-p "SCAN/RATIONAL/F" "SCAN/RATIONAL"))))
               `(progn
                  (serapeum:-> ,name
                               ((simple-array * (*))
                                ,@(if with-key-p
                                      `((function (number) (values number &optional)))))
                               (values (simple-array * (*)) &optional))
                  (declaim (inline ,name))
                  (defun ,name (xs ,@(if with-key-p '(f)))
                    (let* ((length (length xs))
                           (result (make-array length :element-type (array-element-type xs))))
                      (loop with sum = 0
                            for i below (length xs) do
                            (incf sum ,(if with-key-p '(funcall f (aref xs i)) '(aref xs i)))
                            (setf (aref result i) sum))
                      result))))))
  (define-scan nil)
  (define-scan t))
          

(serapeum:-> scan ((simple-array * (*)) &key
                   (:key   (or function null)))
             (values (simple-array * (*)) &optional))
(declaim (inline scan))
(defun scan (xs &key (key nil has-key-p))
  "Compute prefix sum of elements in a vector XS, applying F to each element before summation.

(let ((a (make-array 4 :element-type 'single-float :initial-element 1.0)))
  (vector-sum:scan a))
;; => #(1.0 2.0 3.0 4.0)

(let ((a (make-array 4 :element-type 'single-float :initial-element 1.0)))
  (vector-sum:scan a :key (lambda (x) (* x 2))))
;; => #(2.0 4.0 6.0 8.0)"
  (let ((type (array-element-type xs)))
    (if has-key-p
        (funcall
         (cond
           ((equalp type 'single-float)
            #'scan/single-float/f)
           ((equalp type 'double-float)
            #'scan/double-float/f)
           ((equalp type '(complex single-float))
            #'scan/complex-single-float/f)
           ((equalp type '(complex double-float))
            #'scan/complex-double-float/f)
           (t
            #'scan/rational/f))
         xs key)
        (funcall
         (cond
           ((equalp type 'single-float)
            #'scan/single-float)
           ((equalp type 'double-float)
            #'scan/double-float)
           ((equalp type '(complex single-float))
            #'scan/complex-single-float)
           ((equalp type '(complex double-float))
            #'scan/complex-double-float)
           (t
            #'scan/rational))
         xs))))
