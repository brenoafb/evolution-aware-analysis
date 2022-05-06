#lang racket

(require plot)
(require plot/no-gui)
(provide vector-zip
         vector-range
         make-points
         id)

(define (vector-zip u v)
  (define L (min (vector-length u)
                 (vector-length v)))
  (define zipped (make-vector L))
  (for ((i L))
    (let ((x (vector-ref u i))
          (y (vector-ref v i)))
      (vector-set! zipped i (list x y))))
  zipped)

(define (vector-range lo hi)
  (define L (- hi lo))
  (define v (make-vector L))
  (for ((i L))
    (vector-set! v i (+ lo i)))
  v)

(define (make-points vec)
  (define x (vector-range 1 (add1 (vector-length vec))))
  (vector-zip x vec))

; (define points-baseline
;   (make-points hit-baseline))
;
; (define points-method
;   (make-points hit-method))

(define (id x)
  x)
;
; (parameterize ([plot-y-transform  log-transform]
;                [plot-y-ticks      (log-ticks)])
;   (let ((y-max (* 3
;                   (max
;                    (vector-argmax id hit-baseline)
;                    (vector-argmax id hit-method)))))
;     (plot-file
;      (list
;       (lines points-baseline #:label "baseline" #:color 1)
;       (lines points-method #:label "method" #:color 3))
;      "plots/countEven-1.svg"
;      #:title "countEven"
;      #:x-label "Nodes inserted"
;      #:y-label "Nodes accessed"
;      #:y-max y-max)))
