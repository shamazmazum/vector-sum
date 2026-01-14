(defpackage vector-sum
  (:use #:cl)
  (:export
   ;; v0 API
   #:sum #:scan
   ;; v1 API
   #:sum-state
   #:state-sum
   #:add
   ;; v2 API
   #:sum-state/single-float
   #:sum-state/double-float
   #:sum-state/complex-single-float
   #:sum-state/complex-double-float))
(in-package :vector-sum)
