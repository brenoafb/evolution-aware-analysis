#lang racket

(provide (struct-out tree))
(provide make-tree-leaf)
(provide tree-is-leaf?)
(provide tree-is-node?)
(provide tree-insert-balanced!)

(struct tree
  ([value #:mutable]
   [left #:mutable]
   [right #:mutable])
  #:transparent)

(define (make-tree-leaf value)
  (tree value null null))

(define (tree-is-leaf? tree)
  (and
   (null? (tree-left tree))
   (null? (tree-right tree))))

(define (tree-is-node? tree)
  (not (tree-is-leaf? tree)))

(define (nop x)
  #t)

(define (tree-insert-balanced! tree value [callback nop])
  (begin
    (callback tree)
    (cond ((<= value (tree-value tree))
           (if (null? (tree-left tree))
               (set-tree-left! tree (make-tree-leaf value))
               (tree-insert-balanced! (tree-left tree) value callback)))
          (else
           (if (null? (tree-right tree))
               (set-tree-right! tree (make-tree-leaf value))
               (tree-insert-balanced! (tree-right tree) value callback))))))
