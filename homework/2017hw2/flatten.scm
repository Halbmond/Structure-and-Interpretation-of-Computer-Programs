#lang racket



;(define my-flatten flatten)

#|
if lst = 1, you'll get an error

(define (my-flatten lst)
	(cond
		[(null? lst) '()]
		[(not (list? (car lst)))
			(append
				(list (car lst))
				(my-flatten (cdr lst))
			)
		]
		[else
			(append
				(my-flatten (car lst))
				(my-flatten (cdr lst))
			)
		]
	)
)
|#


(define (my-flatten lst)
	(cond
		[(null? lst) '()]
		[(not (pair? lst)) (list lst)]
		[else
			(append
				(my-flatten (car lst))
				(my-flatten (cdr lst))
			)
		]
	)
)


(define (main)
	(let ([lst (read)])
		(if (eq? lst eof)
			(void)
			(begin
				;(displayln (flatten lst))
				(displayln (my-flatten lst))
				(main)
			)
		)
	)
)


(main)


#|
样例输入
(1 (2 3) (4 (tom 6) x) (8 9) 10)
(1)
((((2))))
((((2)) a) 6)
()
(() 1)
样例输出
(1 2 3 4 tom 6 x 8 9 10)
(1)
(2)
(2 a 6)
()
(1)
|#
