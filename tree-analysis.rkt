#lang racket

(require racket/hash)
(require racket/pretty)
(require profile)
(require math/array)
(require plot)
(require plot/no-gui)
(require "tree.rkt")
(require "array-utils.rkt")
(require "meta.rkt")
(require "plotting.rkt")

(provide tree-count-even
         tree-count-even-m)

(define iterations 1000)
(define repetitions 10)
(define starting-size 1000)

; (define sleep-duration 0.0000001)
(define sleep-duration
  (make-parameter 0))

(define tree-node-hit 0)

(define (tree-value-log tree)
  (set! tree-node-hit (add1 tree-node-hit))
  (tree-value tree))

(define (tree-count-even tree)
  (sleep (sleep-duration))
  (cond ((null? tree) 0)
        ((even? (tree-value-log tree))
         (+ 1
            (tree-count-even (tree-left tree))
            (tree-count-even (tree-right tree))))
        (else
         (+ 0
            (tree-count-even (tree-left tree))
            (tree-count-even (tree-right tree))))))

(define (tree-count-even-m tree)
  (cond ((null? tree) 0)
        ((even? (tree-value-log tree))
         `(+ 1 (tree-count-even ,(tree-left tree)) (tree-count-even ,(tree-right tree))))
        (else
         `(+ 0 (tree-count-even ,(tree-left tree)) (tree-count-even ,(tree-right tree))))))

(define ns (make-base-namespace))

(define (make-tree-of-size n [max 100000])
  (let ((new-tree (make-tree-leaf (random max))))
    (for/list ([i n])
      (tree-insert-balanced! new-tree (random max)))
    new-tree))


; (benchmark-count-free-vars iterations)

(define hit-baseline
  (make-vector iterations))

(define hit-method
  (make-vector iterations))

(define time-baseline
  (make-array-1 iterations repetitions))

(define time-method
  (make-array-1 iterations repetitions))

(define (test-baseline)
  (define new-tree (make-tree-of-size starting-size))
  (for ((i iterations))
    (for ((r repetitions))
      (define start-time (current-inexact-monotonic-milliseconds))
      (tree-insert-balanced! new-tree (random 100000))
      (set! tree-node-hit 0)
      (tree-count-even new-tree)
      (define end-time (current-inexact-monotonic-milliseconds))
      (array-set-index! time-baseline i r (- end-time start-time))
      (vector-set! hit-baseline i tree-node-hit))))

(define (test-method)
  (define ctx (make-hasheq))
  (define (callback tree)
    (hash-remove! ctx tree))
  (define new-tree (make-tree-of-size starting-size))
  ; (set-box! (tree-count-even) (hasheq)) ; clear cache
  (for ((i iterations))
    (for ((r repetitions))
      (define start-time (current-inexact-monotonic-milliseconds))
      (tree-insert-balanced! new-tree (random 100000) callback)
      (set! tree-node-hit 0)
      (let* ((bctx (make-hasheq))
             (fctx (make-hasheq))
             (expr (unfold! ctx bctx 'tree-count-even tree-count-even-m `(tree-count-even ,new-tree)))
             (result (eval-lift bctx fctx expr)))
        (begin
          (define end-time (current-inexact-monotonic-milliseconds))
          ; (println (format "expr: ~a" expr))
          (if (= r (- repetitions 1))
              (update-ctx ctx fctx)
              '())
          (array-set-index! time-method i r (- end-time start-time))
          (vector-set! hit-method i tree-node-hit))))))

(define (test)
  (define ctx (make-hasheq))
  (define (callback expr)
    (hash-remove! ctx expr))
  (define new-tree (make-tree-of-size starting-size))
  (for ((i iterations))
    (if (zero? (modulo i 100)) (println (format "~a" i)) '())
    (for ((r repetitions))
      (define evolution-start-time (current-inexact-monotonic-milliseconds))
      (tree-insert-balanced! new-tree (random 100000) callback)
      (define evolution-end-time (current-inexact-monotonic-milliseconds))
      (define evolution-time (- evolution-start-time evolution-end-time))

      (set! tree-node-hit 0)
      (define start-time (current-inexact-monotonic-milliseconds))
      (iteration-base new-tree)
      (define end-time (current-inexact-monotonic-milliseconds))
      (vector-set! hit-baseline i tree-node-hit)
      (array-set-index! time-baseline i r (- end-time start-time))

      (set! tree-node-hit 0)
      (set! start-time (current-inexact-monotonic-milliseconds))
      (iteration-method new-tree ctx r)
      (set! end-time (current-inexact-monotonic-milliseconds))

      (vector-set! hit-method i tree-node-hit)
      (array-set-index! time-method i r (+ evolution-time (- end-time start-time))))))

(define (iteration-base tree)
  (tree-count-even tree))

(define (iteration-method tree ctx r)
  (let* ((bctx (make-hasheq))
         (fctx (make-hasheq))
         (expr (unfold! ctx bctx 'tree-count-even tree-count-even-m `(tree-count-even ,tree)))
         (result (eval-lift bctx fctx expr)))
    (begin
      (if (= r (- repetitions 1))
          (update-ctx ctx fctx)
          '()))))

(define (test-time)
  (println "Running")
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
   "plots/time-countEven.svg"
   #:title "tree-count-even - Execution time"
   #:x-label "Nodes inserted"
   #:y-label "Analysis time (ms)")
  (define hit-points-baseline (make-points hit-baseline))
  (define hit-points-method (make-points hit-method))

  (parameterize ([plot-y-transform  log-transform]
                 [plot-y-ticks      (log-ticks)])
    (plot-file
     (list
      (lines hit-points-baseline #:label "baseline" #:color 1)
      (lines hit-points-method #:label "method" #:color 3))
     "plots/hit-countEven.svg"
     #:title "tree-count-even - Node accesses"
     #:x-label "Nodes inserted"
     #:y-label "Input nodes accessed"))
  (for ((item (list (list time-baseline "time-baseline")
                    (list time-method   "time-method")
                    (list hit-baseline  "hit-baseline")
                    (list hit-method    "hit-method"))))
    (define out
      (open-output-file
       (format "experimental-data/tree-count-even-~a.txt" (cadr item))
       #:exists 'replace))
    (display (car item) out)
    (close-output-port out)))

(define (test-correctness)
  (define ctx (make-hasheq))
  (define (callback tree)
    (hash-remove! ctx tree))
  (define new-tree (make-tree-of-size starting-size))
  (for ((i iterations))
    (tree-insert-balanced! new-tree (random 100000) callback)
    (set! tree-node-hit 0)
    (let* ((bctx (make-hasheq))
           (fctx (make-hasheq))
           (expr (unfold! ctx bctx 'tree-count-even tree-count-even-m `(tree-count-even ,new-tree)))
           (result (eval-lift bctx fctx expr))
           (base-result (tree-count-even new-tree)))
      (if (not (eq? result base-result))
          (println "ERROR")
          (begin
            (println (format "result matched: ~a vs ~a (original)" result base-result))
            (update-ctx ctx fctx)
            (vector-set! hit-method i tree-node-hit))))))

; (test-correctness)
(test-time)
