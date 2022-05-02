#lang racket


(require racket/hash)

(require racket/pretty)

(require "meta.rkt")
(require "lisp-analysis.rkt")
(require "array-utils.rkt")
(require "plotting.rkt")

(require profile)

(provide expr-depth
         expr-depth-m
         expr-depth)

(define iterations 1000)
(define repetitions 10)

(define sleep-duration 0)

(define expr-hit 0)

(define hit-baseline
  (make-vector iterations))

(define hit-method
  (make-vector iterations))

(define time-baseline
  (make-array-1 iterations repetitions))

(define time-method
  (make-array-1 iterations repetitions))

(define (expr-car-log expr)
  (set! expr-hit (add1 expr-hit))
  (mcar expr))

(define (expr-cdr-log expr)
  (set! expr-hit (add1 expr-hit))
  (mcdr expr))

(define (expr-depth expr)
  (set! expr-hit (add1 expr-hit))
  (cond ((null? expr) 0)
        ((mpair? expr)
         (+ (+ 1 (expr-depth (expr-car-log expr)))
            (expr-depth (expr-cdr-log expr))))
        (else 0)))

(define (expr-depth-m expr)
  (set! expr-hit (add1 expr-hit))
  (cond ((null? expr) 0)
        ((mpair? expr)
         `(+ (+ 1 (expr-depth ,(expr-car-log expr))) (expr-depth ,(expr-cdr-log expr))))
        (else 0)))

(define ns (make-base-namespace))

(require plot)
(require plot/no-gui)

(define points-baseline
  (make-points hit-baseline))

(define points-method
  (make-points hit-method))

; (plot-new-window? #t)

(define (id x)
  x)

(define (test-baseline)
  (define program (list-to-mlist (read-from-file "test.rkt")))
  (for ((i iterations))
    (for ((r repetitions))
      (define start-time (current-inexact-monotonic-milliseconds))
      (mutate-expr-rand! program)
      (set! expr-hit 0)
      (expr-depth program)
      (define end-time (current-inexact-monotonic-milliseconds))
      (array-set-index! time-baseline i r (- end-time start-time))
      (vector-set! hit-baseline i expr-hit))))

(define (test-initial-iteration)
  (define time-initial (make-array-1 1 repetitions))
  (define ctx (make-hasheq))
  (define (callback expr)
    (hash-remove! ctx expr))
  (define program (list-to-mlist (read-from-file "test.rkt")))
  (for ((r repetitions))
    (println (format "~a" r))
    (define start-time (current-inexact-monotonic-milliseconds))
    (iteration-method program ctx r)
    (define end-time (current-inexact-monotonic-milliseconds))
    (array-set-index! time-initial 0 r (- end-time start-time)))
  time-initial)

(define (test)
  (define ctx (make-hasheq))
  (define (callback expr)
    (hash-remove! ctx expr))
  (define program (list-to-mlist (read-from-file "test.rkt")))
  (iteration-method program ctx (- repetitions 1)) ; prepopulate cache
  (for ((i iterations))
    (if (zero? (modulo i 100)) (println (format "~a" i)) '())
    (for ((r repetitions))
      (define mutation-start-time (current-inexact-monotonic-milliseconds))
      (mutate-expr-rand! program callback)
      (define mutation-end-time (current-inexact-monotonic-milliseconds))
      (define mutation-time (- mutation-start-time mutation-end-time))

      (set! expr-hit 0)
      (define start-time (current-inexact-monotonic-milliseconds))
      (iteration-base program)
      (define end-time (current-inexact-monotonic-milliseconds))
      (vector-set! hit-baseline i expr-hit)
      (array-set-index! time-baseline i r (- end-time start-time))

      (set! expr-hit 0)
      (set! start-time (current-inexact-monotonic-milliseconds))
      (iteration-method program ctx r)
      (set! end-time (current-inexact-monotonic-milliseconds))

      (vector-set! hit-method i expr-hit)
      (array-set-index! time-method i r (+ mutation-time (- end-time start-time))))))

(define (iteration-base program)
  (expr-depth program))

(define (iteration-method program ctx r)
  (let* ((bctx (make-hasheq))
         (fctx (make-hasheq))
         (expr (unfold! ctx bctx 'expr-depth expr-depth-m `(expr-depth ,program)))
         (result (eval-lift bctx fctx expr)))
    (begin
      (if (= r (- repetitions 1))
          (update-ctx ctx fctx)
          '()))))

(define (test-time)
  (println "Testing")
  (test)
  (define avg-time-baseline
    (make-avgs time-baseline iterations repetitions))
  (define avg-time-method
    (make-avgs time-method iterations repetitions))
  (define time-points-baseline (make-points avg-time-baseline))
  (define time-points-method (make-points avg-time-method))
  (plot-file
   (list
    (lines time-points-baseline #:label "baseline" #:color 1)
    (lines time-points-method #:label "method" #:color 3))
   "plots/time-expr-depth.svg"
   #:title "expr-depth - Execution time"
   #:x-label "Iterations"
   #:y-label "Analysis time (ms)")
  (define hit-points-baseline (make-points hit-baseline))
  (define hit-points-method (make-points hit-method))

  (parameterize ([plot-y-transform  log-transform]
                 [plot-y-ticks      (log-ticks)])
    (plot-file
     (list
      (lines hit-points-baseline #:label "baseline" #:color 1)
      (lines hit-points-method #:label "method" #:color 3))
     "plots/hit-expr-depth.svg"
     #:title "expr-depth - Expression accesses"
     #:x-label "Iterations"
     #:y-label "Expresion node accesses"))
  (for ((item (list (list time-baseline "time-baseline")
                    (list time-method   "time-method")
                    (list hit-baseline  "hit-baseline")
                    (list hit-method    "hit-method"))))
    (define out
      (open-output-file
       (format "experimental-data/expr-depth-~a.txt" (cadr item))
       #:exists 'replace))
    (display (car item) out)
    (close-output-port out)))

(define (test-correctness)
  (println "testing correctness")
  (define program (list-to-mlist (read-from-file "test.rkt")))
  (define ctx (make-hasheq))
  (define (callback expr)
    (hash-remove! ctx expr))
  (let ((method-result (expr-depth program))
        (base-result (expr-depth program)))
    (if (not (eq? method-result base-result))
        'FAIL
        (for ((i iterations))
          (mutate-expr-rand! program callback)
          (set! expr-hit 0)
          (let* ((bctx (make-hasheq))
                 (fctx (make-hasheq))
                 (expr (unfold! ctx bctx 'expr-depth expr-depth-m `(expr-depth ,program)))
                 (result (eval-lift bctx fctx expr))
                 (base-result (expr-depth program)))
            (if (not (eq? result base-result))
                (begin
                  (println "ERROR")
                  (println (format "result: ~a" result))
                  (println (format "base result: ~a" base-result)))
                (begin
                  (update-ctx ctx fctx)
                  (vector-set! hit-method i expr-hit))))))))

; (test-time)

(test-initial-iteration)
; (test-correctness)

; (define program
;   (list-to-mlist '(+ 1 (+ 2 (+ 3 (+ 4 5))))))
;
; (set! expr-hit 0)
;
; (define base-result (expr-depth program))
;
; ;
; (set-box! (expr-depth) (hasheq)) ; clear cache
;
; (define ctx (unbox (expr-depth)))
;
; (define bctx (make-hasheq))
;
; (define fctx (make-hasheq))
;
; (define expr (unfold! ctx bctx 'expr-depth expr-depth-m `(expr-depth ,program)))
;
; (define result (eval-lift bctx fctx expr))
; result
; base-result
;
;
; (set! ctx (update-ctx ctx fctx))
;
; ctx
;
; bctx
;
;
; fctx
;
; expr
;
; (hash-ref ctx program)
; (hash-ref ctx (mcdr (mcdr (mcdr program))))
;
;
; (define method-result (expr-depth program))

;
; (mutate-expr-rand! program callback)
;
; (define ctx (unbox (expr-depth)))
;
; (define bctx (make-hasheq))
;
; (define fctx (make-hasheq))
;
; (define expr (unfold! ctx bctx 'expr-depth expr-depth-m `(expr-depth ,program)))
;
; (define result (eval-lift bctx fctx expr))
;
; (define base-result (expr-depth program))
;
; (define new-ctx (update-ctx ctx fctx))
