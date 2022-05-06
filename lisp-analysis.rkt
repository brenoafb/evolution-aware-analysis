#lang racket

(require racket/hash)
(require racket/pretty)
(require "meta.rkt")

(provide read-from-file
         list-to-mlist
         nop
         insert-expr!
         wrap-expr
         random-atom
         mutate-expr-rand!)

; (define (max a b)
;   (if (> a b)
;       a
;       b))

(define (read-from-file f)
  (let ((input (open-input-file f)))
    (let loop ((expr (read input))
               (result '()))
      (if (eof-object? expr)
          (reverse result)
          (loop (read input) (cons expr result))))))

(define (list-to-mlist l)
  (cond ((null? l) '())
        ((pair? l)
         (mcons
          (list-to-mlist (car l))
          (list-to-mlist (cdr l))))
        (else l)))

(define (mlist-to-list l)
  (cond ((null? l) '())
        ((pair? l)
         (cons
          (mlist-to-list (mcar l))
          (mlist-to-list (mcdr l))))
        (else l)))

(define (nop x)
  #t)

(define (insert-expr! e1 e2 [callback nop])
  (callback e1)
  (cond ((null? e1) e2)
        ((null? (mcdr e1))
         (set-mcdr! e1 e2))
        (else (insert-expr! (mcdr e1) e2))))

(define (wrap-expr e)
  (list-to-mlist `(,(random-atom) ,(random-expr) ,e)))

(define (random-atom)
  'mutation)

(define maximum-mutation-depth 10)

(define (random-expr)
  (define (loop n)
    (if (= n 0)
        (random-atom)
        (let ((r (random 100)))
          (cond
            ((< r 33) (random-atom))
            ((< r 66) `(,(random-atom) ,(random-atom)))
            (else `(,(loop (- n 1)) ,(loop (- n 1))))))))
  (list-to-mlist
   (loop (random maximum-mutation-depth))))

(define (mutate-expr-rand! e [callback nop])
  (callback e)
  (cond ((null? e) e)
        ((and (mpair? e) (null? (mcdr e)))
         (set-mcdr! e (random-expr)))
        ((not (mpair? e)) (wrap-expr e))
        (else
         (let ((r (random 100)))
           (cond ((< r 80) (mutate-expr-rand! (mcdr e) callback)) ; traverse remainder of expression
                 ((< r 90)
                  (let* ((xs (mcdr e))                            ; wrap current node in some operation
                         (new-node (wrap-expr xs)))
                    (set-mcdr! e new-node)))
                 (else                                            ; insert new node here
                  (let* ((xs (mcdr e))
                         (new-node (mcons (random-expr) xs)))
                    (set-mcdr! e new-node))))))))
