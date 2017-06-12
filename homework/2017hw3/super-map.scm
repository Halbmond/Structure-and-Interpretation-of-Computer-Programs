#lang racket
(define (map op lst)
	(if (null? lst)
			'()
			(cons (op (car lst))
						(map op (cdr lst)))))

(define (super-map op . w)
;MY CODE BEGIN
	(cons (apply op (map car w))
		(if (null? (cdar w))
			null
			(apply super-map (cons op (map cdr w)))
		)
	)
)
;MY CODE END

(define (myloop)
	(let ((a (read))
				(b (read))
				(c (read)))
		(if (eq? a eof)
				(void)
				(begin (displayln (super-map + a b c)) 
							 (displayln (super-map (lambda (x y) (+ x (* 2 y) )) a b ))
							 (myloop)))))
(myloop)
