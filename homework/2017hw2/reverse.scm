#lang racket


;(define my-reverse reverse)

(define (my-reverse lst)
	(cond
		[(null? lst) '()]
		[else
			(append
				(my-reverse (cdr lst))
				(list (car lst))
			)
		]
	)
)

(define (main)
	(let ([lst (read)])
		(if (eq? lst eof)
			(void)
			(begin
				(displayln (my-reverse lst))
				(main)
			)
		)
	)
)

(main)
