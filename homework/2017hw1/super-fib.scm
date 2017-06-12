#|
#lang lazy
(define (f f1 f2 f3 f4 f5) (+	f1(* 4 f2)(* 5 f3)(* -2 f4 f4)(* f5 f5 f5)))
(define fibs (list* 1 1 1 1 1 (map f(cddddr fibs)(cdddr fibs)(cddr fibs)(cdr fibs)fibs)))
(define (loop) (displayln (list-ref fibs (read))) (loop))
(loop)
|#
#lang lazy
(define F(list* 1 1 1 1 1(map(lambda(A B C D E)(+	A(* 4 B)(* 5 C)(* -2 D D)(* E E E)))(cddddr F)(cdddr F)(cddr F)(cdr F)F)))
(define (L)(define n(read))(unless (eq? n eof)(displayln(list-ref F n))(L)))
(L)
#|
#lang lazy
(define fibs
	(list* 1 1 1 1 1
		(map (lambda (f1 f2 f3 f4 f5) (+	f1(* 4 f2)(* 5 f3)(* -2 f4 f4)(* f5 f5 f5)))
			(cddddr fibs)(cdddr fibs)(cddr fibs)(cdr fibs)fibs)))
(define (loop) (define n (read)) (unless (eq? n eof) (displayln (list-ref fibs n)) (loop)))
(loop)
|#