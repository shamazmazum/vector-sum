(defpackage vector-sum
  (:use #:cl)
  (:export
   ;; v0 API
   #:sum #:scan
   ;; v1 API
   #:sum-state
   #:state-sum
   #:add))
(in-package :vector-sum)
