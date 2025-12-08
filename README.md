# vector-sum

This is a small library for vector summation. It uses Kahan summation if a
vector's element type is irrational:

``` lisp
CL-USER> (defparameter *xs* (make-array 600000 :element-type 'single-float :initial-element 0.01))
*XS*
CL-USER> (reduce #'+ *xs*)
5958.9736
CL-USER> (vector-sum:sum *xs*)
6000.0
```

The function `sum` supports `:start`, `:end` and `:key` parameters which have
the same meaning as in `reduce`.

There is also `scan` function for prefix sums:

``` lisp
CL-USER> (defparameter *xs* (make-array 4 :element-type 'single-float :initial-contents '(1.0 2.0 3.0 4.0)))
*xs*
CL-USER> (vector-sum:scan *xs*)
#(1.0 3.0 6.0 10.0)
```

## New function from version 1.0 API

Now there is a generic `add` function which adds a value to a state.

E.g. this way you can calculate `1f-6 * n`:

``` lisp
(serapeum:-> foo ((integer 0))
             (values single-float &optional))
(defun foo (n)
  (declare (optimize (speed 3)))
  ;; SUM-STATE is specialized over its argument which can be any
  ;; number.
  (let ((state (vector-sum:sum-state 0f0)))
    (labels ((%go (state n)
               (declare (type fixnum n))
               (if (zerop n) state
                   ;; ADD is specialized over the second argument. The
                   ;; state must be "compatible" with the second
                   ;; argument, i.e. you cannot add a single float
                   ;; value to a double-float state.
                   (%go (vector-sum:add state 1f-6)
                        (1- n)))))
      ;; STATE-SUM extracts the result from the state.
      (vector-sum:state-sum
       (%go state n)))))
```

The result is
``` lisp
CL-USER> (loop repeat 10000 sum 1f-6)
0.009999673
CL-USER> (foo 10000)
0.01
```
