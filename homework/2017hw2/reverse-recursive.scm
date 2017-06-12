#lang racket

#|
(define (rev_rec x)
	(reverse (map (lambda (x) (if (list? x) (rev_rec x) x)) x))
)
|#
(define (reverse-recursive lst)
	(cond
		[(not (pair? lst)) lst]
		[else
			(append
				(reverse-recursive (cdr lst))
				(list (reverse-recursive (car lst)))
			)
		]
	)
)

(define (main)
	(let ((lst (read)))
		(if (eq? lst eof)
			(void)
			(begin
				(displayln (reverse-recursive lst))
				(main)
			)
		)
	)
)

(main)

