(in-package :vector-sum)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun type-name (type)
    (cond
      ((atom type)
       (symbol-name type))
      ((eq (first type) 'complex)
       (format nil "COMPLEX-~a" (second type)))
      (t
       (error "Something wrong")))))

(macrolet ((def-sum-state (type init-val)
             (let ((type-name (intern (format nil "SUM-STATE/~a"    (type-name type))))
                   (init-name (intern (format nil "+INIT-STATE/~a+" (type-name type)))))
               `(progn
                  (eval-when (:compile-toplevel :load-toplevel :execute)
                    (serapeum:defconstructor ,type-name
                      (sum   ,type)
                      (error ,type)))
                  (alexandria:define-constant ,init-name
                      (,type-name ,init-val ,init-val)
                    :test #'equalp)))))
  (def-sum-state single-float 0f0)
  (def-sum-state double-float 0d0)
  (def-sum-state (complex single-float) (complex 0f0))
  (def-sum-state (complex double-float) (complex 0d0))
  (def-sum-state number 0))

(deftype sum-state ()
  '(or
    sum-state/single-float
    sum-state/double-float
    sum-state/complex-single-float
    sum-state/complex-double-float
    sum-state/number))

(serapeum:-> sum-state (number)
             (values sum-state &optional))
(declaim (inline sum-state))
(defun sum-state (x)
  "Create a summator for numbers which have the same type as X."
  (cond
    ((typep x 'single-float)
     +init-state/single-float+)
    ((typep x 'double-float)
     +init-state/double-float+)
    ((typep x '(complex single-float))
     +init-state/complex-single-float+)
    ((typep x '(complex double-float))
     +init-state/complex-double-float+)
    (t
     +init-state/number+)))

(serapeum:-> state-sum (sum-state)
             (values number &optional))
(declaim (inline state-sum))
(defun state-sum (state)
  "Return the result of summation from the state."
  (cond
    ((typep state 'sum-state/single-float)
     (sum-state/single-float-sum state))
    ((typep state 'sum-state/double-float)
     (sum-state/double-float-sum state))
    ((typep state 'sum-state/complex-single-float)
     (sum-state/complex-single-float-sum state))
    ((typep state 'sum-state/complex-double-float)
     (sum-state/complex-double-float-sum state))
    ((typep state 'sum-state/number)
     (sum-state/number-sum state))))

(macrolet ((def-add (state-type x-type)
             (let ((fn-name    (intern (format nil "ADD/~a" (type-name x-type))))
                   (sum-name   (intern (format nil "SUM-STATE/~a-SUM"   (type-name x-type))))
                   (error-name (intern (format nil "SUM-STATE/~a-ERROR" (type-name x-type)))))
               `(progn
                  (serapeum:-> ,fn-name (,state-type ,x-type)
                               (values ,state-type &optional))
                  (declaim (inline ,fn-name))
                  (defun ,fn-name (state x)
                    (let* ((sum   (,sum-name   state))
                           (error (,error-name state))
                           (y (- x error))
                           (tmp (+ sum y)))
                      (,state-type tmp (- tmp sum y))))))))
  (def-add sum-state/single-float single-float)
  (def-add sum-state/double-float double-float)
  (def-add sum-state/complex-single-float (complex single-float))
  (def-add sum-state/complex-double-float (complex double-float))
  (def-add sum-state/number number))


(serapeum:-> add (sum-state number)
             (values sum-state &optional))
(declaim (inline add))
(defun add (state x)
  "Add X to the result of summation in the state."
  (cond
    ((typep x 'single-float)
     (add/single-float state x))
    ((typep x 'double-float)
     (add/double-float state x))
    ((typep x '(complex single-float))
     (add/complex-single-float state x))
    ((typep x '(complex double-float))
     (add/complex-double-float state x))
    (t
     (add/number state x))))
