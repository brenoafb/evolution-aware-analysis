#lang racket

(require math/array)

(provide array-set-index!
         array-ref-index
         make-array-1
         make-avgs)

(define (array-set-index! arr i j x)
  (let ((idx (list->vector (list i j))))
    (array-set! arr idx x)))

(define (array-ref-index arr i j)
  (let ((idx (list->vector (list i j))))
    (array-ref arr idx)))

(define (make-array-1 m n)
  (array->mutable-array
   (make-array (list->vector (list m n)) 0)))

(define (make-avgs arr m n)
  (define avg (make-vector m))
  (for ((i m))
    (define sum 0)
    (for ((j n))
      (set! sum (+ sum (array-ref-index arr i j))))
    (vector-set! avg i (exact->inexact (/ sum n))))
  avg)
