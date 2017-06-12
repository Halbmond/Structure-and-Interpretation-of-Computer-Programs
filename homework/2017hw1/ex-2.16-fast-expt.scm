#lang racket


;(define (even? n) (= (remainder n 2) 0))

(define (power a n)
	(cond ((= n 0) 1)
		((even? n) (sqr (power a (/ n 2))))
		(else (* a (power a (sub1 n))))
	)
)

(define (main)
	(let ([a (read)] [n (read)])
		(if (eq? a eof)
			(void)
			(begin
				(displayln (power a n))
				(main)
			)
		)
	)
)

(main)