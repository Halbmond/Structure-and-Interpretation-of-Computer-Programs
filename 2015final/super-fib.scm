#lang lazy
(define (f f1 f2 f3 f4 f5) (+	f1(* 4 f2)(* 5 f3)(* -2 f4 f4)(* f5 f5 f5)))
(define fibs (list* 1 1 1 1 1 (map f(cddddr fibs)(cdddr fibs)(cddr fibs)(cdr fibs)fibs)))
(define (loop) (displayln (list-ref fibs (read))) (loop))
(loop)
