#lang racket


(define (read-list)
	(let ((x (read)))
		(if (eq? x eof)
			'()
			(cons x (read-list))
		)
	)
)


(for ([x (remove-duplicates
			(sort (read-list) <)
		)
	])
	(printf "~a " x)
)