#lang racket

;wrong
;(define cmp (lambda (x y) (string<? (format "~a" x) (format "~a" y))))

(define (cmp x y) (cond
	[(null? x) #t]
	[(null? y) #f]
	[(< (car x) (car y)) #t]
	[(> (car x) (car y)) #f]
	[else (cmp (cdr x) (cdr y))]))

(define (combinations a) (if (eq? a '()) '(())
	(let ((A (combinations (cdr a))))
		(append A (map (lambda (b) (cons (car a) b)) A))))
)

(let loop ([a (read)])
	(unless (eq? a eof)
		(displayln (sort (combinations (sort a <)) cmp))
		(loop (read))
	)
)

#|
样例输入
()
(1)
(18 2)
(2 3 14)
样例输出
(())
(() (1))
(() (2) (2 18) (18))
(() (2) (2 3) (2 3 14) (2 14) (3) (3 14) (14))
|#
