#lang racket

(require racket/hash)
(require racket/pretty)
(provide reduce!
         unfold!
         eval-lift
         update-ctx)

(define (reduce! ctx bctx f meta-f expr)
  (cons
   (car expr)
   (map (lambda (x) (unfold! ctx bctx f meta-f x)) (cdr expr))))

(define ns (make-base-namespace))

(define (unfold! ctx bctx f meta-f expr)
  (if (func-app? expr)
      (cond ((null? expr) expr)
            ((eq? (car expr) f)
             (let ((arg (cadr expr)))
               (if (in-ctx? ctx arg)
                   (ctx-get ctx arg)
                   (let ((result
                          (unfold! ctx bctx f meta-f (apply meta-f (list arg)))))
                     (begin
                       (hash-set! bctx result arg)
                       result)))))
            (else (reduce! ctx bctx f meta-f expr)))
      expr))

(define (eval-lift bctx fctx expr)
  (if (not (hash-has-key? bctx expr))
      (eval expr ns)
      (let ((src (hash-ref bctx expr)))
        (cond ((hash-has-key? fctx src)
               ; we already computed this expr
               (hash-ref fctx src))
              ; haven't computed expr yet
              ((list? expr)
               (let*
                   ((new-expr (map (lambda (x) (eval-lift bctx fctx x)) expr))
                    (result (eval new-expr ns)))
                   (begin
                     (hash-set! fctx src result)
                     result)))
              (else
               (let ((result (eval expr ns)))
                 (begin
                   (hash-set! fctx src result)
                   result)))))))

(define (update-ctx ctx fctx)
  (hash-union! ctx fctx #:combine (lambda (x y) y)))

(define (eval-key-value-pair x y)
  (cons x (eval y ns)))


(define (func-app? expr)
  (if (null? expr)
      #f
      (list? expr)))


(define (in-ctx? ctx x)
  (hash-has-key? ctx x))

(define (ctx-get ctx x)
  (hash-ref ctx x))

(define (my-function)
  (for ((i 100000000))
    (add1 1))
  #t)
