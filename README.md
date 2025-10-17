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
